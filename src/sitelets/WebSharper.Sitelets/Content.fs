// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2016 IntelliFactory
//
// Licensed under the Apache License, Version 2.0 (the "License"); you
// may not use this file except in compliance with the License.  You may
// obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied.  See the License for the specific language governing
// permissions and limitations under the License.
//
// $end{copyright}

namespace WebSharper.Sitelets

open System.IO
open WebSharper
module CT = WebSharper.Core.ContentTypes

[<CompiledName "FSharpContent">]
type Content<'Action> =
    | CustomContent of (Context<'Action> -> Http.Response)
    | CustomContentAsync of (Context<'Action> -> Async<Http.Response>)

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Content =
    open System
    open System.Collections.Generic
    open System.IO
    open System.Text.RegularExpressions
    open System.Web

    type private Func<'A,'B> = System.Func<'A,'B>

    module M = WebSharper.Core.Metadata
//    module R = WebSharper.Core.Reflection
    module J = WebSharper.Core.Json

    let metaJson<'T> (m: M.Info) (jP: Core.Json.Provider) (controls: seq<IRequiresResources>) =
        controls
        |> List.ofSeq
        |> List.collect (fun c -> c.Encode(m, jP))
        |> J.Encoded.Object
        |> jP.Pack
        |> J.Stringify

    let escape (s: string) =
        Regex.Replace(s, @"[&<>']",
            new MatchEvaluator(fun m ->
                match m.Groups.[0].Value.[0] with
                | '&'-> "&amp;"
                | '<'-> "&lt;"
                | '>' -> "&gt;"
                | '\'' -> "&#39;"
                | _ -> failwith "unreachable"))

    type Env =
        {
            AppPath : string
            Json : Core.Json.Provider
            Meta : Core.Metadata.Info
            Graph : Core.DependencyGraph.Graph 
            ResourceContext : Core.Resources.Context
        }

        static member Create<'T>(ctx: Context<'T>) =
            {
                AppPath = ctx.ApplicationPath
                Json = ctx.Json
                Meta = ctx.Metadata
                Graph = ctx.Dependencies
                ResourceContext = ctx.ResourceContext
            }

    let writeResources (env: Env) (controls: seq<#IRequiresResources>) (tw: Core.Resources.RenderLocation -> UI.HtmlTextWriter) =
        // Resolve resources for the set of types and this assembly
        let resources =
            let nodeSet =
                controls
                |> Seq.collect (fun c -> c.Requires)
                |> Set
            env.ResourceContext.ResourceDependencyCache.GetOrAdd(nodeSet, fun nodes ->
                env.Graph.GetResources nodes
            )
        let hasResources = not (List.isEmpty resources)
        if hasResources then
            // Meta tag encoding the client side controls
            let mJson = metaJson env.Meta env.Json (Seq.cast controls)
            // Render meta
            (tw Core.Resources.Meta).WriteLine(
                "<meta id='{0}' name='{0}' content='{1}' />",
                Activator.META_ID,
                escape mJson
            )
            // Render resources
            for r in resources do
                Core.Resources.Rendering.RenderCached(env.ResourceContext, r, tw)
        hasResources

    let writeStartScript (tw: UI.HtmlTextWriter) =
        tw.WriteLine(@"<script type='{0}'>", CT.Text.JavaScript.Text)
        tw.WriteLine @"if (typeof IntelliFactory !=='undefined')"
        tw.WriteLine @"  IntelliFactory.Runtime.Start();"
        tw.WriteLine @"</script>"

    type RenderedResources =
        {
            Scripts : string
            Styles : string
            Meta : string
        }

        member this.Item
            with get (x: string) =
                match x.ToLowerInvariant() with
                | "scripts" -> this.Scripts
                | "styles" -> this.Styles
                | "meta" -> this.Meta
                | _ -> failwith "Invalid rendered resource identifier"

    let getSeparateResourcesAndScripts env controls : RenderedResources =
        use scriptsW = new StringWriter()
        let scriptsTw = new UI.HtmlTextWriter(scriptsW, " ")
        use stylesW = new StringWriter()
        let stylesTw = new UI.HtmlTextWriter(stylesW, " ")
        use metaW = new StringWriter()
        let metaTw = new UI.HtmlTextWriter(metaW, " ")
        let hasResources =
            writeResources env controls (function
                | Core.Resources.Scripts -> scriptsTw
                | Core.Resources.Styles -> stylesTw
                | Core.Resources.Meta -> metaTw)
        if hasResources then
            writeStartScript scriptsTw
        {
            Scripts = scriptsW.ToString()
            Styles = stylesW.ToString()
            Meta = metaW.ToString()
        }

    let getResourcesAndScripts env controls =
        use w = new StringWriter()
        use tw = new UI.HtmlTextWriter(w, " ")
        let hasResources = writeResources env controls (fun _ -> tw)
        if hasResources then writeStartScript tw
        w.ToString()

    type Env with
        member this.GetSeparateResourcesAndScripts controls =
            getSeparateResourcesAndScripts this controls
        member this.GetResourcesAndScripts controls =
            getResourcesAndScripts this controls

    let toCustomContentAsync (htmlPage: Page) : Context<'T> -> Async<Http.Response> =
        () // Force a 1-arg function returning a func, rather than a 2-arg function
        fun context -> async {
            let writeBody (stream: Stream) =
                let body = Seq.cache htmlPage.Body
                let renderHead (tw: UI.HtmlTextWriter) =
                    let hasResources = writeResources (Env.Create context) body (fun _ -> tw)
                    for elem in htmlPage.Head do
                        elem.Write(context.Metadata, tw)
                    if hasResources then writeStartScript tw
                let renderBody (tw: UI.HtmlTextWriter) =
                    for elem in body do
                        elem.Write(context.Metadata, tw)
                // Create html writer from stream
                use textWriter = new StreamWriter(stream)
                textWriter.AutoFlush <- true
                use htmlWriter = new System.Web.UI.HtmlTextWriter(textWriter, " ")
                htmlPage.Renderer htmlPage.Doctype htmlPage.Title
                    renderHead renderBody htmlWriter
            return {
                Status = Http.Status.Ok
                Headers = [Http.Header.Custom "Content-Type" "text/html; charset=utf-8"]
                WriteBody = writeBody
            }
        }

    let JsonContent<'T, 'U> (f: Context<'T> -> 'U) =
        let encoder = ActionEncoding.JsonProvider.GetEncoder<'U>()
        Content.CustomContent <| fun ctx ->
            let x = f ctx
            {
                Status = Http.Status.Ok
                Headers = [Http.Header.Custom "Content-Type" "application/json"]
                WriteBody = fun s ->
                    use tw = new StreamWriter(s)
                    x
                    |> encoder.Encode
                    |> ActionEncoding.JsonProvider.Pack
                    |> WebSharper.Core.Json.Write tw
            }

    let JsonContentAsync<'T, 'U> (f: Context<'T> -> Async<'U>) =
        let encoder = ActionEncoding.JsonProvider.GetEncoder<'U>()
        Content.CustomContentAsync <| fun ctx ->
            async {
                let! x = f ctx
                return {
                    Status = Http.Status.Ok
                    Headers = [Http.Header.Custom "Content-Type" "application/json"]
                    WriteBody = fun s ->
                        use tw = new StreamWriter(s)
                        x
                        |> encoder.Encode
                        |> ActionEncoding.JsonProvider.Pack
                        |> WebSharper.Core.Json.Write tw
                }
            }

    let ToResponse<'T> (c: Content<'T>) (ctx: Context<'T>) : Async<Http.Response> =
        match c with
        | CustomContent x -> async { return x ctx }
        | CustomContentAsync x -> x ctx

    let FromContext f =
        Content.CustomContentAsync (fun ctx -> async {
            let! content = f ctx
            return! ToResponse content ctx
        })
        |> async.Return

    let ToResponseAsync c ctx = ToResponse c ctx

    let FromAsync ac =
        CustomContentAsync <| fun ctx -> async {
            let! c = ac
            return! ToResponse c ctx
        }

    let delay1 f =
        fun arg -> async { return f arg }

    let MapResponseAsync<'T> (f: Http.Response -> Async<Http.Response>) (content: Async<Content<'T>>) =
        let genResp content =
            match content with
            | CustomContent gen -> delay1 gen
            | CustomContentAsync x -> x
        CustomContentAsync <| fun context ->
            async {
                let! content = content
                let! result = genResp content context
                return! f result
            }
        |> async.Return

    let MapResponse<'T> (f: Http.Response -> Http.Response) (content: Async<Content<'T>>) =
        let genResp content =
            match content with
            | CustomContent gen -> delay1 gen
            | CustomContentAsync x -> x
        CustomContentAsync <| fun context ->
            async {
                let! content = content
                let! result = genResp content context
                return f result
            }
        |> async.Return

    let MapContextSync<'T> (f: Context<'T> -> Context<'T>) (content: Content<'T>) : Content<'T> =
        match content with
        | CustomContent c -> CustomContent (f >> c)
        | CustomContentAsync c -> CustomContentAsync (f >> c)

    let MapContext<'T> (f:Context<'T> -> Context<'T>) (content: Async<Content<'T>>) : Async<Content<'T>> =
        async {
            let! content = content
            return MapContextSync f content
        }

    let SetHeaders<'T> (headers: seq<Http.Header>) (cont: Async<Content<'T>>) =
        cont
        |> MapResponse (fun resp -> { resp with Headers = headers })

    let WithHeaders<'T> (headers: seq<Http.Header>) (cont: Async<Content<'T>>) =
        cont
        |> MapResponse (fun resp ->
            let headers = (List.ofSeq headers) @ (List.ofSeq resp.Headers)
            { resp with Headers = headers }
        )

    let WithHeader<'T> (name: string) (value: string) (cont: Async<Content<'T>>) =
        cont |> WithHeaders [Http.Header.Custom name value]

    let WithContentType<'T> (contentType: string) (cont: Async<Content<'T>>) =
        cont |> WithHeaders [Http.Header.Custom "Content-Type" contentType]

    let SetStatus<'T> (status: Http.Status) (cont: Async<Content<'T>>) =
        cont
        |> MapResponse (fun resp -> { resp with Status = status })

    let SetBody<'T> (writeBody: System.IO.Stream -> unit) (cont: Async<Content<'T>>) =
        cont
        |> MapResponse (fun resp -> { resp with WriteBody = writeBody })

    /// Emits a 301 Moved Permanently response to a given URL.
    let RedirectToUrl<'T> (url: string) : Content<'T> =
        CustomContent <| fun ctx ->
            {
                Status = Http.Status.Custom 301 (Some "Moved Permanently")
                Headers = [Http.Header.Custom "Location" url]
                WriteBody = ignore
            }

    /// Emits a 301 Moved Permanently response to a given action.
    let Redirect<'T> (action: 'T) =
        CustomContentAsync <| fun ctx ->
            let resp = RedirectToUrl (ctx.Link action)
            ToResponse resp ctx

    let RedirectPermanentToUrl url = RedirectToUrl url |> async.Return
    let RedirectPermanent url = Redirect url |> async.Return

    /// Emits a 307 Redirect Temporary response to a given url.
    let RedirectTemporaryToUrl<'T> (url: string) : Async<Content<'T>> =
        CustomContent <| fun ctx ->
            {
                Status = Http.Status.Custom 307 (Some "Temporary Redirect")
                Headers = [Http.Header.Custom "Location" url]
                WriteBody = ignore
            }
        |> async.Return

    /// Emits a 307 Redirect Temporary response to a given url.
    let RedirectTemporary<'T> (action: 'T) : Async<Content<'T>> =
        CustomContentAsync <| fun ctx -> async {
            let! content = RedirectTemporaryToUrl (ctx.Link action)
            return! ToResponse content ctx
        }
        |> async.Return

    /// Constructs a status code response.
    let httpStatusContent<'T> status : Async<Content<'T>> =
        CustomContent <| fun ctx ->
            {
                Status = status
                Headers = []
                WriteBody = ignore
            }
        |> async.Return

    let Unauthorized<'T> : Async<Content<'T>> =
        httpStatusContent Http.Status.Unauthorized

    let Forbidden<'T> : Async<Content<'T>> =
        httpStatusContent Http.Status.Forbidden

    let NotFound<'T> : Async<Content<'T>> =
        httpStatusContent Http.Status.NotFound

    let NotImplemented<'T> : Async<Content<'T>> =
        httpStatusContent Http.Status.NotImplemented

    let ServerError<'T> : Async<Content<'T>> =
        httpStatusContent Http.Status.InternalServerError

    let MethodNotAllowed<'T> : Async<Content<'T>> =
        httpStatusContent Http.Status.MethodNotAllowed

type Content<'Action> with

    static member Custom (response: Http.Response) : Async<Content<'Action>> =
        Content.CustomContent <| fun _ -> response
        |> async.Return

    static member Custom (?Status: Http.Status, ?Headers: seq<Http.Header>, ?WriteBody: System.IO.Stream -> unit) : Async<Content<'Action>> =
        ({
            Status = defaultArg Status Http.Status.Ok
            Headers = defaultArg Headers Seq.empty
            WriteBody = defaultArg WriteBody ignore
        } : Http.Response)
        |> Content.Custom

    static member Json x : Async<Content<'Action>> =
        Content.JsonContent <| fun _ -> x
        |> async.Return

    static member Page (?Body: #seq<#WebSharper.Web.INode>, ?Head:#seq<#WebSharper.Web.INode>, ?Title: string, ?Doctype: string) : Async<Content<'Action>> =
        Content.Page {
            Doctype = Some (match Doctype with Some d -> d | None -> "<!DOCTYPE html>")
            Title = Title
            Head = match Head with None -> Seq.empty | Some x -> Seq.cast x
            Body = match Body with None -> Seq.empty | Some x -> Seq.cast x
            Renderer = Page.Default.Renderer
        }

    static member Page (page: Page) : Async<Content<'Action>> =
        Content.CustomContentAsync (Content.toCustomContentAsync page)
        |> async.Return

    static member Text (text: string, ?encoding: System.Text.Encoding) : Async<Content<'Action>> =
        let encoding = defaultArg encoding System.Text.Encoding.UTF8
        Content.Custom(
            WriteBody = fun s ->
                use w = new System.IO.StreamWriter(s, encoding)
                w.Write(text)
        )

    static member File (path: string, ?AllowOutsideRootFolder: bool, ?ContentType) : Async<Content<'Action>> =
        let allowOutsideRootFolder = defaultArg AllowOutsideRootFolder false
        Content.CustomContent <| fun ctx ->
            if Path.IsPathRooted path && not allowOutsideRootFolder then
                failwith "Cannot serve file from outside the application's root folder"
            let rootFolder = DirectoryInfo(ctx.RootFolder).FullName
            let path =
                if path.StartsWith "~/" || path.StartsWith @"~\" then
                    Path.Combine(rootFolder, path.[2..])
                else
                    Path.Combine(rootFolder, path)
            let fi = System.IO.FileInfo(path)
            if fi.FullName.StartsWith rootFolder || allowOutsideRootFolder then
                {
                    Status = Http.Status.Ok
                    Headers = [if ContentType.IsSome then yield Http.Header.Custom "Content-Type" ContentType.Value]
                    WriteBody = fun out ->
                        use inp = fi.OpenRead()
                        let buffer = Array.zeroCreate (16 * 1024)
                        let rec loop () =
                            let read = inp.Read(buffer, 0, buffer.Length)
                            if read > 0 then out.Write(buffer, 0, read); loop ()
                        loop ()
                }
            else
                failwith "Cannot serve file from outside the application's root folder"
        |> async.Return

open System
open System.Threading.Tasks
open System.Runtime.InteropServices
open System.Runtime.CompilerServices

#nowarn "49" // allow uppercase parameter names

[<AutoOpen>]
module Internal =

    type Task<'A> with
        member t.Map (f: 'A -> 'B) =
            t.ContinueWith(fun (t: Task<'A>) -> f t.Result)

[<CompiledName "Content"; Struct; Extension; NoEquality; NoComparison>]
type CSharpContent =

    val private c: Content<obj>

    new(c) = { c = c }

    static member ToContent (c: Async<Content<obj>>) : Task<CSharpContent> =
        Async.StartAsTask(c)
            .ContinueWith(fun (t: Task<Content<obj>>) -> CSharpContent t.Result)

    static member Opt x =
        match box x with
        | null -> None
        | _ -> Some x

    member this.AsContent = this.c

    static member TaskAsContent (t: Task<CSharpContent>) =
        t.Map(fun c -> c.AsContent)

    static member Json<'U> (x: 'U) : Task<CSharpContent> =
        Content.Json x
        |> CSharpContent.ToContent

    static member Page
        (
            [<Optional>] Body: Web.INode,
            [<Optional>] Head: Web.INode,
            [<Optional>] Title: string,
            [<Optional>] Doctype: string
        ) =
        Content.Page(
            ?Body = Option.map Seq.singleton (CSharpContent.Opt Body),
            ?Head = Option.map Seq.singleton (CSharpContent.Opt Head),
            ?Title = CSharpContent.Opt Title, ?Doctype = CSharpContent.Opt Doctype)
        |> CSharpContent.ToContent

    static member Page (page: Page) =
        Content.Page(page)
        |> CSharpContent.ToContent

    static member Text (text: string, [<Optional>] encoding: System.Text.Encoding) =
        Content.Text(text, ?encoding = CSharpContent.Opt encoding)
        |> CSharpContent.ToContent

    static member File (path: string, [<Optional>] AllowOutsideRootFolder: bool, [<Optional>] ContentType: string) =
        Content.File(path, AllowOutsideRootFolder, ?ContentType = CSharpContent.Opt ContentType)
        |> CSharpContent.ToContent

    static member Custom (response: Http.Response) =
        Content.Custom(response)
        |> CSharpContent.ToContent

    static member Custom
        (
            [<Optional>] Status: Http.Status,
            [<Optional>] Headers: seq<Http.Header>,
            [<Optional>] WriteBody: Action<Stream>
        ) =
        Content.Custom(?Status = CSharpContent.Opt Status, ?Headers = CSharpContent.Opt Headers, WriteBody = WriteBody.Invoke)
        |> CSharpContent.ToContent

    static member FromContext (f: Func<Context, Task<CSharpContent>>) =
        Content.FromContext (fun x -> Async.AwaitTask (f.Invoke(Context x).Map(fun c -> c.AsContent)))
        |> CSharpContent.ToContent

    [<Extension>]
    static member ToResponse (content: CSharpContent, context: Context) =
        Content.ToResponse content.AsContent context
        |> Async.StartAsTask

    static member FromTask (content: Task<CSharpContent>) =
        content
        |> CSharpContent.TaskAsContent
        |> Async.AwaitTask
        |> Content.FromAsync
        |> CSharpContent

    [<Extension>]
    static member MapResponse (content: Task<CSharpContent>, f: Func<Http.Response, Http.Response>) =
        content
        |> CSharpContent.TaskAsContent
        |> Async.AwaitTask
        |> Content.MapResponse f.Invoke
        |> CSharpContent.ToContent

    [<Extension>]
    static member MapResponseAsync (content: Task<CSharpContent>, f: Func<Http.Response, Task<Http.Response>>) =
        content
        |> CSharpContent.TaskAsContent
        |> Async.AwaitTask
        |> Content.MapResponseAsync (fun r -> Async.AwaitTask (f.Invoke r))
        |> CSharpContent.ToContent

    [<Extension>]
    static member WithHeaders (content: Task<CSharpContent>, headers: seq<Http.Header>) =
        content
        |> CSharpContent.TaskAsContent
        |> Async.AwaitTask
        |> Content.WithHeaders headers
        |> CSharpContent.ToContent

    [<Extension>]
    static member WithHeaders (content: Task<CSharpContent>, [<ParamArray>] headers: Http.Header[]) =
        content
        |> CSharpContent.TaskAsContent
        |> Async.AwaitTask
        |> Content.WithHeaders headers
        |> CSharpContent.ToContent

    [<Extension>]
    static member WithHeader (content: Task<CSharpContent>, name: string, value: string) =
        content
        |> CSharpContent.TaskAsContent
        |> Async.AwaitTask
        |> Content.WithHeader name value
        |> CSharpContent.ToContent

    [<Extension>]
    static member WithContentType (content: Task<CSharpContent>, contentType: string) =
        content
        |> CSharpContent.TaskAsContent
        |> Async.AwaitTask
        |> Content.WithContentType contentType
        |> CSharpContent.ToContent

    [<Extension>]
    static member SetHeaders (content: Task<CSharpContent>, headers: seq<Http.Header>) =
        content
        |> CSharpContent.TaskAsContent
        |> Async.AwaitTask
        |> Content.SetHeaders headers
        |> CSharpContent.ToContent

    [<Extension>]
    static member SetHeaders (content: Task<CSharpContent>, [<ParamArray>] headers: Http.Header[]) =
        content
        |> CSharpContent.TaskAsContent
        |> Async.AwaitTask
        |> Content.SetHeaders headers
        |> CSharpContent.ToContent

    [<Extension>]
    static member SetStatus (content: Task<CSharpContent>, status: Http.Status) =
        content
        |> CSharpContent.TaskAsContent
        |> Async.AwaitTask
        |> Content.SetStatus status
        |> CSharpContent.ToContent

    [<Extension>]
    static member SetStatus (content: Task<CSharpContent>, code: int, [<Optional>] message: string) =
        content
        |> CSharpContent.TaskAsContent
        |> Async.AwaitTask
        |> Content.SetStatus (Http.Status.Custom code (CSharpContent.Opt message))
        |> CSharpContent.ToContent

    [<Extension>]
    static member SetBody (content: Task<CSharpContent>, writeBody: Action<Stream>) =
        content
        |> CSharpContent.TaskAsContent
        |> Async.AwaitTask
        |> Content.SetBody writeBody.Invoke
        |> CSharpContent.ToContent

    static member RedirectPermanent (action: obj) =
        Content.RedirectPermanent action
        |> CSharpContent.ToContent

    static member RedirectPermanentToUrl (url: string) =
        Content.RedirectPermanentToUrl url
        |> CSharpContent.ToContent

    static member RedirectTemporary (action: obj) =
        Content.RedirectTemporary action
        |> CSharpContent.ToContent

    static member RedirectTemporaryToUrl (url: string) =
        Content.RedirectTemporaryToUrl url
        |> CSharpContent.ToContent

    static member Unauthorized() =
        Content.Unauthorized
        |> CSharpContent.ToContent

    static member Forbidden() =
        Content.Forbidden
        |> CSharpContent.ToContent

    static member NotFound() =
        Content.NotFound
        |> CSharpContent.ToContent

    static member ServerError() =
        Content.ServerError
        |> CSharpContent.ToContent

    static member MethodNotAllowed() =
        Content.MethodNotAllowed
        |> CSharpContent.ToContent
