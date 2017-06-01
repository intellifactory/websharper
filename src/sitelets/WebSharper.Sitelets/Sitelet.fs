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

#nowarn "44" // Obsolete CustomContent, CustomContentAsync, PageContent, PageContentAsync

open System
open System.Collections.Generic
open System.Web.UI

type Sitelet<'T when 'T : equality> =
    {
        Router : Router<'T>
        Controller : Controller<'T>
    }

    static member ( <|> ) (s1: Sitelet<'Action>, s2: Sitelet<'Action>) =
        {
            Router = s1.Router <|> s2.Router
            Controller =
                {
                    Handle = fun action ->
                        match s1.Router.Link action with
                        | Some _ -> s1.Controller.Handle action
                        | None -> s2.Controller.Handle action
                }
        }

/// Provides combinators over sitelets.
module Sitelet =
    open Microsoft.FSharp.Quotations
    open Microsoft.FSharp.Reflection
    module C = Content
    type C<'T> = Content<'T>

    /// Creates an empty sitelet.
    let Empty<'Action when 'Action : equality> : Sitelet<'Action> =
        {
            Router = Router.New (fun _ -> None) (fun _ -> None)
            Controller =
                {
                    Handle = fun action ->
                        Content.CustomContent <| fun _ ->
                            {
                                Status = Http.Status.NotFound
                                Headers = []
                                WriteBody = ignore
                            }
                }
        }

    /// Represents filters for protecting sitelets.
    type Filter<'Action> =
        {
            VerifyUser : string -> bool;
            LoginRedirect : 'Action -> 'Action
        }

    /// Constructs a protected sitelet given the filter specification.
    let Protect (filter: Filter<'Action>) (site: Sitelet<'Action>)
        : Sitelet<'Action> =
        {
            Router = site.Router
            Controller =
                { Handle = fun action ->
                    let prot = filter
                    let failure ctx = async {
                        let! c = Content.RedirectTemporary (prot.LoginRedirect action)
                        return! Content.ToResponse c ctx
                    }
                    Content.CustomContentAsync <| fun ctx ->
                        async {
                            try
                                let! loggedIn = ctx.UserSession.GetLoggedInUser ()
                                match loggedIn with
                                | Some user ->
                                    if prot.VerifyUser user then
                                        let content = site.Controller.Handle action
                                        return! Content.ToResponse content ctx
                                    else
                                        return! failure ctx
                                | None ->
                                    return! failure ctx
                            with :? NullReferenceException ->
                                // If server crashes or is restarted and doesn't have a hardcoded machine
                                // key then GetLoggedInUser() throws an exception. Log out in this case.
                                do! ctx.UserSession.Logout()
                                return! failure ctx
                        }
                }
        }

    /// Constructs a singleton sitelet that contains exactly one action
    /// and serves a single content value at a given location.
    let Content (location: string) (action: 'Action) (cnt: Context<'Action> -> Async<Content<'Action>>) =
        {
            Router = Router.Table [action, location]
            Controller = { Handle = fun _ ->
                Content.CustomContentAsync <| fun ctx -> async {
                    let! cnt = cnt ctx
                    return! Content.ToResponse cnt ctx
                }
            }
        }

    /// Maps over the sitelet action type. Requires a bijection.
    let Map (f: 'T1 -> 'T2) (g: 'T2 -> 'T1) (s: Sitelet<'T1>) : Sitelet<'T2> =
        {
            Router = Router.Map f g s.Router
            Controller =
                {
                    Handle = fun action ->
                        match s.Controller.Handle <| g action with
                        | Content.CustomContent genResp ->
                            CustomContent (genResp << Context.Map f)
                        | Content.CustomContentAsync genResp ->
                            CustomContentAsync (genResp << Context.Map f)
                }
        }

    let MapContext (f: Context<'T> -> Context<'T>) (s: Sitelet<'T>) : Sitelet<'T> =
        { s with
            Controller =
                {
                    Handle = fun action ->
                        s.Controller.Handle action
                        |> WebSharper.Sitelets.Content.MapContextSync f
                }
        }

    /// Maps over the sitelet action type with only an injection.
    let Embed embed unembed sitelet =
        {
            Router = Router.TryMap (Some << embed) unembed sitelet.Router
            Controller =
                { Handle = fun a ->
                    match unembed a with
                    | Some ea -> C.CustomContentAsync <| fun ctx ->
                        C.ToResponse (sitelet.Controller.Handle ea) (Context.Map embed ctx)
                    | None -> failwith "Invalid action in Sitelet.Embed" }
        }

    let tryGetEmbedFunctionsFromExpr (expr: Expr<'T1 -> 'T2>) =
        match expr with
        | ExprShape.ShapeLambda(_, Patterns.NewUnionCase (uci, _)) ->
            let embed (y: 'T1) = FSharpValue.MakeUnion(uci, [|box y|]) :?> 'T2
            let unembed (x: 'T2) =
                let uci', args' = FSharpValue.GetUnionFields(box x, uci.DeclaringType)
                if uci.Tag = uci'.Tag then
                    Some (args'.[0] :?> 'T1)
                else None
            Some (embed, unembed)
        | _ -> None
 
    /// Maps over the sitelet action type, where the destination type
    /// is a discriminated union with a case containing the source type.
    let EmbedInUnion (case: Expr<'T1 -> 'T2>) sitelet =
        match tryGetEmbedFunctionsFromExpr case with
        | Some (embed, unembed) -> Embed embed unembed sitelet
        | None -> failwith "Invalid union case in Sitelet.EmbedInUnion"

    /// Shifts all sitelet locations by a given prefix.
    let Shift (prefix: string) (sitelet: Sitelet<'T>) =
        {
            Router = Router.Shift prefix sitelet.Router
            Controller = sitelet.Controller
        }

    /// Combines several sitelets, leftmost taking precedence.
    /// Is equivalent to folding with the choice operator.
    let Sum (sitelets: seq<Sitelet<'T>>) : Sitelet<'T> =
        if Seq.isEmpty sitelets then Empty else
            Seq.reduce (<|>) sitelets

    /// Serves the sum of the given sitelets under a given prefix.
    /// This function is convenient for folder-like structures.
    let Folder<'T when 'T : equality> (prefix: string)
                                      (sitelets: seq<Sitelet<'T>>) =
        Shift prefix (Sum sitelets)

    /// Boxes the sitelet action type to Object type.
    let Upcast (sitelet: Sitelet<'T>) : Sitelet<obj> =
        Map box unbox sitelet

    /// Reverses the Upcast operation on the sitelet.
    let UnsafeDowncast<'T when 'T : equality> (sitelet: Sitelet<obj>) : Sitelet<'T> =
        Map unbox box sitelet

    /// Constructs a sitelet with an inferred router and a given controller
    /// function.
    let Infer<'T when 'T : equality> (handle : Context<'T> -> 'T -> Async<Content<'T>>) =
        {
            Router = Router.Infer()
            Controller = { Handle = fun x ->
                C.CustomContentAsync <| fun ctx -> async {
                    let! content = handle ctx x
                    return! C.ToResponse content ctx
                }
            }
        }

    let InferWithCustomErrors<'T when 'T : equality> (handle : Context<'T> -> ActionEncoding.DecodeResult<'T> -> Async<Content<'T>>) =
        {
            Router = Router.InferWithErrors<'T>()
            Controller = { Handle = fun x ->
                C.CustomContentAsync <| fun ctx -> async {
                    let ctx = (Context.Map ActionEncoding.Success ctx)
                    let! content = handle ctx x
                    return! C.ToResponse content ctx
                }
            }
        }

    let InferPartial (embed: 'T1 -> 'T2) (unembed: 'T2 -> 'T1 option) (mkContent: Context<'T2> -> 'T1 -> Async<Content<'T2>>) : Sitelet<'T2> =
        {
            Router = Router.Infer() |> Router.TryMap (Some << embed) unembed
            Controller = { Handle = fun p ->
                C.CustomContentAsync <| fun ctx -> async {
                    match unembed p with
                    | Some e ->
                        let! content = mkContent ctx e
                        return! C.ToResponse content ctx
                    | None -> return failwith "Invalid action in Sitelet.InferPartial"
                }
            }
        }

    let InferPartialInUnion (case: Expr<'T1 -> 'T2>) mkContent =
        match tryGetEmbedFunctionsFromExpr case with
        | Some (embed, unembed) -> InferPartial embed unembed mkContent
        | None -> failwith "Invalid union case in Sitelet.InferPartialInUnion"

type Sitelet<'T> with
    member internal this.Upcast =
        Sitelet.Upcast this

open System.Threading.Tasks

type SiteletBuilder() =

    let mutable sitelets = []

    member this.With<'T when 'T : equality>(content: Func<Context, 'T, Task<CSharpContent>>) =
        let sitelet =
            Sitelet.InferPartial
                box
                (function :? 'T as x -> Some x | _ -> None)
                (fun ctx action -> async {
                    let! content =
                        content.Invoke(Context(ctx), action)
                        |> Async.AwaitTask
                    return
                        match content.AsContent with
                        | CustomContent f -> CustomContent (f << Context.Map box)
                        | CustomContentAsync f -> CustomContentAsync (f << Context.Map box)
                })
        sitelets <- sitelet :: sitelets
        this

    member this.With(path: string, content: Func<Context, Task<CSharpContent>>) =
        let content ctx =
            content.Invoke(Context(ctx))
                .ContinueWith(fun (t: Task<CSharpContent>) -> t.Result.AsContent)
            |> Async.AwaitTask
        sitelets <- Sitelet.Content path (box path) content :: sitelets
        this

    member this.Install() =
        Sitelet.Sum (List.rev sitelets)
