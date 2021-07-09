// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2018 IntelliFactory
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

module WebSharper.Compiler.CommandTools

open System
open System.IO
open System.Reflection
open WebSharper
open WebSharper.Compiler
open WebSharper.Core

exception ArgumentError of msg: string with
    override this.Message = this.msg
let argError msg = raise (ArgumentError msg)

type ProjectType =
    | Bundle
    | BundleOnly
    | Website
    | Html
    | WIG
    | Proxy

    static member Parse(wsProjectType: string) =
        match wsProjectType.ToLower() with
        | "" | "ignore" | "library" -> None
        | "bundle" -> Some Bundle
        | "bundleonly" -> Some BundleOnly
        | "extension" | "interfacegenerator" -> Some WIG
        | "html" -> Some Html
        | "site" | "web" | "website" | "export" -> Some Website
        | "proxy" -> Some Proxy
        | _ -> argError ("Invalid project type: " + wsProjectType)

type JavaScriptScope =
    | JSDefault
    | JSAssembly
    | JSFilesOrTypes of string[]
 
type WsConfig =
    {
        SourceMap   : bool
        TypeScript  : bool
        IsDebug : bool
        ProjectType : ProjectType option
        OutputDir  : string option
        ScriptBaseUrl : string option
        AssemblyFile : string
        References  : string[] 
        Resources : (string * string option)[]
        KeyFile : string option
        CompilerArgs : string[]        
        ProjectFile : string
        Documentation : string option
        VSStyleErrors : bool
        PrintJS : bool
        WarnOnly : bool
        DeadCodeElimination : bool
        DownloadResources : bool option
        AnalyzeClosures : bool option
        JavaScriptScope : JavaScriptScope
        JavaScriptExport : JsExport[]
        JSOutputPath : string option
        MinJSOutputPath : string option
        SingleNoJSErrors : bool
        ProxyTargetName : string option
        UseJavaScriptSymbol : bool
        TargetProfile : string
        Standalone : bool
    }

    member this.ProjectDir =
        match Path.GetDirectoryName this.ProjectFile with
        | "" -> Directory.GetCurrentDirectory()
        | p -> p

    static member Empty =
        {                 
            SourceMap   = false
            TypeScript  = false
            IsDebug = false
            ProjectType = None
            OutputDir  = None
            ScriptBaseUrl = None
            AssemblyFile = null
            References  = [||]
            Resources = [||]
            KeyFile = None
            CompilerArgs = [||]
            ProjectFile = null
            Documentation = None
            VSStyleErrors = false
            PrintJS  = false
            WarnOnly = false
            DeadCodeElimination = true
            DownloadResources = None       
            AnalyzeClosures = None
            JavaScriptScope = JSDefault
            JavaScriptExport = [||]
            JSOutputPath = None
            MinJSOutputPath = None
            SingleNoJSErrors = false
            ProxyTargetName = None
            UseJavaScriptSymbol = false
            TargetProfile = "mscorlib"
            Standalone = System.Environment.GetEnvironmentVariables()
                |> Seq.cast<System.Collections.DictionaryEntry>
                |> Seq.exists (fun x -> (x.Key :?> string).ToLower() = "websharperbuildservice" && (x.Value :?> string).ToLower() = "false")
        }

    static member ParseAnalyzeClosures(c: string) =
        match c.ToLower() with
        | "true" -> Some false
        | "movetotop" -> Some true
        | _ -> argError "Invalid value for AnalyzeClosures, value must be true or movetotop."
    
    member this.AddJson(jsonString, fileName) =
        let json =
            try Json.Parse jsonString 
            with _ -> argError (sprintf "Failed to parse %s, not a valid json." fileName)
        let settings = 
            match json with
            | Json.Object values -> values
            | _ -> argError (sprintf "Failed to parse %s, not a json object." fileName)
        let getString k v =
            match v with
            | Json.String s -> s
            | _ -> argError (sprintf "Invalid value in %s for %s, expecting a string." fileName k)
        let projectDir = Path.GetDirectoryName this.ProjectFile
        let getPath k v =
            Path.Combine(projectDir, getString k v)
        let getBool k v = 
            match v with
            | Json.True -> true
            | Json.False -> false
            | Json.String s ->
                match bool.TryParse s with
                | true, b -> b
                | _ -> argError (sprintf "Invalid value in %s for %s, expecting true or false." fileName k)
            | _ -> argError (sprintf "Invalid value in %s for %s, expecting true or false." fileName k)
        let getDlRes k v = 
            match v with
            | Json.True -> Some true
            | Json.False -> None
            | Json.String s ->
                match bool.TryParse s with
                | true, b -> if b then Some true else None
                | _ ->
                    match s.ToLower() with
                    | "warnonly" -> Some false
                    | _ -> argError (sprintf "Invalid value in %s for %s, expecting true or false or 'warnonly'." fileName k)   
            | _ -> argError (sprintf "Invalid value in %s for %s, expecting true or false or 'warnonly'." fileName k)
        let mutable res = this
        for k, v in settings do
            match k.ToLower() with
            | "project" ->
                res <- { res with ProjectType = ProjectType.Parse (getString k v) }
            | "outputdir" ->
                res <- { res with OutputDir = Some (getPath k v) }
            | "scriptbaseurl" ->
                res <- { res with ScriptBaseUrl = Some (getString k v) }
            | "dce" ->
                res <- { res with DeadCodeElimination = getBool k v }
            | "sourcemap" ->
                res <- { res with SourceMap = getBool k v }
            | "warnonly" ->
                res <- { res with WarnOnly = getBool k v }
            | "downloadresources" ->
                res <- { res with DownloadResources = getDlRes k v }
            | "analyzeclosures" ->
                let a =
                    match v with
                    | Json.True -> Some false
                    | Json.False -> None
                    | Json.String s -> WsConfig.ParseAnalyzeClosures s
                    | _ -> argError (sprintf "Invalid value in %s for AnalyzeClosures, value must be true, false or \"movetotop\"." fileName)    
                res <- { res with AnalyzeClosures = a }
            | "javascript" ->
                let j =
                    match v with
                    | Json.True -> JSAssembly
                    | Json.False -> JSDefault
                    | Json.Array a ->
                        a |> Seq.map (
                            function
                            | Json.String s -> s
                            | _ -> argError (sprintf "Invalid value in %s for JavaScript, expecting true or false or an array of strings." fileName)
                        ) |> Array.ofSeq |> JSFilesOrTypes
                    | _ -> argError (sprintf "Invalid value in %s for JavaScript, expecting true or false or an array of strings." fileName) 
                res <- { res with JavaScriptScope = j }
            | "javascriptexport" ->
                let j =
                    match v with
                    | Json.True -> [| ExportCurrentAssembly |]
                    | Json.False -> [||]
                    | Json.Array a ->
                        a |> Seq.map (
                            function
                            | Json.True -> ExportCurrentAssembly
                            | Json.String s -> ExportByName s
                            | _ -> argError (sprintf "Invalid value in %s for JavaScriptExport, expecting true or false or an array of strings." fileName)
                        ) |> Array.ofSeq
                    | _ -> argError (sprintf "Invalid value in %s for JavaScriptExport, expecting true or false or an array of strings." fileName) 
                res <- { res with JavaScriptExport = Array.append this.JavaScriptExport j }
            | "jsoutput" ->
                res <- { res with JSOutputPath = Some (getPath k v) }
            | "minjsoutput" ->
                res <- { res with MinJSOutputPath = Some (getPath k v) }
            | "singlenojserrors" ->
                res <- { res with SingleNoJSErrors = getBool k v }
            | "proxytargetname" ->
                res <- { res with ProxyTargetName = Some (getString k v) }
            | "usejavascriptsymbol" ->
                res <- { res with UseJavaScriptSymbol = getBool k v }
            | "$schema" -> ()
            | _ -> failwithf "Unrecognized setting in %s: %s" fileName k 
        res
    
module ExecuteCommands =
    
    let TryGetOutputDir settings = 
        settings.OutputDir |> Option.map (fun o ->
            Path.Combine(Path.GetDirectoryName settings.ProjectFile, o)
        )                                                        

    let GetWebRoot settings =
        match TryGetOutputDir settings with
        | None ->
            let dir = settings.ProjectDir
            let isWeb =
                File.Exists(Path.Combine(dir, "Web.config"))
                || File.Exists(Path.Combine(dir, "web.config"))
            if isWeb then Some dir else None
        | Some out -> Some out

    let BundleOutputDir settings webRoot =
        match TryGetOutputDir settings with
        | None ->
            match webRoot with
            | Some webRoot ->
                let d = Path.Combine(webRoot, "Content")
                let di = DirectoryInfo(d)
                if not di.Exists then
                    di.Create()
                Some d
            | None -> None
        | Some out -> Some out

    let Unpack webRoot settings (logger: LoggerBase) =
        sprintf "Unpacking into %s" webRoot
        |> logger.Out
        for d in ["Scripts/WebSharper"; "Content/WebSharper"] do
            let dir = DirectoryInfo(Path.Combine(webRoot, d))
            if not dir.Exists then
                dir.Create()
        let assemblies =
            let dir =
                match settings.OutputDir with
                | None | Some "" -> Path.Combine(webRoot, "bin")
                | Some p -> p
            [
                yield! Directory.EnumerateFiles(dir, "*.dll")
                yield! Directory.EnumerateFiles(dir, "*.exe")
                yield settings.AssemblyFile
                yield! settings.References
            ]        
            |> List.distinct
        let cfg =
            {
                Compiler.UnpackCommand.Config.Create() with
                    Assemblies = assemblies
                    RootDirectory = webRoot
                    UnpackSourceMap = settings.SourceMap
                    UnpackTypeScript = settings.TypeScript
                    DownloadResources = Option.isSome settings.DownloadResources
            }
        let env = Compiler.Commands.Environment.Create()
        Compiler.UnpackCommand.Instance.Execute(env, cfg)

    let HtmlOutputDirectory settings =
        match TryGetOutputDir settings with
        | None -> Path.Combine(settings.ProjectDir, "bin", "html")
        | Some dir -> dir

    let Html settings (logger: LoggerBase) =
        let outputDir = HtmlOutputDirectory settings
        sprintf "Generating static site into %s" outputDir
        |> logger.Out
        let main = settings.AssemblyFile
        let refs = List.ofArray settings.References
        let cfg =
            {
                Compiler.HtmlCommand.Config.Create(main) with
                    Mode = if settings.IsDebug then Compiler.HtmlCommand.Debug else Compiler.HtmlCommand.Release
                    OutputDirectory = outputDir
                    ProjectDirectory = settings.ProjectDir
                    ReferenceAssemblyPaths = refs
                    UnpackSourceMap = settings.SourceMap
                    UnpackTypeScript = settings.TypeScript
                    DownloadResources = settings.DownloadResources |> Option.defaultValue false
            }
        let env = Compiler.Commands.Environment.Create()
        Compiler.HtmlCommand.Instance.Execute(env, cfg)

let LoadInterfaceGeneratorAssembly (file: string) =
    let genFile = Path.ChangeExtension(file, ".Generator.dll")
    if File.Exists genFile then File.Delete genFile
    File.Copy(file, genFile)
    let asm = Assembly.Load(File.ReadAllBytes(genFile))
    let name = asm.GetName()
    match Attribute.GetCustomAttribute(asm, typeof<InterfaceGenerator.Pervasives.ExtensionAttribute>) with
    | :? InterfaceGenerator.Pervasives.ExtensionAttribute as attr ->
        name, attr.GetAssembly(), asm
    | _ ->
        failwith "No ExtensionAttribute set on the input assembly"

let RunInterfaceGenerator (aR: AssemblyResolver) snk config =
    aR.Wrap <| fun () ->
        let (name, asmDef, asm) = LoadInterfaceGeneratorAssembly config.AssemblyFile
        let cfg =
            {
                InterfaceGenerator.CompilerOptions.Default(name.Name) with
                    AssemblyResolver = Some aR
                    AssemblyVersion = name.Version
                    DocPath = config.Documentation
                    EmbeddedResources = config.Resources |> Seq.map fst
                    ProjectDir = config.ProjectDir
                    ReferencePaths = config.References
                    StrongNameKeyPath = snk
            }

        let cmp = InterfaceGenerator.Compiler.Create()
        let out = cmp.Compile(cfg, asmDef, asm)
        out.Save config.AssemblyFile

let (|StartsWith|_|) (start: string) (input: string) =    
    if input.StartsWith start then
        Some input.[start.Length ..]
    else None 

let (|Flag|_|) name (input: string) =
    if input = name
        || input = name + "+"
        || input = name + ":true"
    then Some true
    elif input = name + "-"
        || input = name + ":false"
    then Some false
    else None

let (|Cmd|_|) (cmd: Commands.ICommand) argv =
    match cmd.Parse argv with
    | Commands.NotRecognized -> None
    | Commands.Parsed f ->
        match f (Commands.Environment.Create()) with
        | Commands.Ok -> Some 0
        | Commands.Errors errors ->
            for e in errors do
                stderr.WriteLine(e)
            Some 1
    | Commands.ParseFailed err ->
        for e in err do
            stderr.WriteLine(e)
        stderr.WriteLine()
        stderr.WriteLine(cmd.Usage)
        Some 1

type private HelpKind =
    | NoHelp
    | WSHelp
    | UnpackHelp
    | HtmlHelp

let HandleDefaultArgsAndCommands (logger: LoggerBase) argv isFSharp =

    let printInfo (logger: LoggerBase) helpKind = 
        let lang = if isFSharp then "F#" else "C#"
        let exe = if isFSharp then "wsfsc.exe" else "zafircs.exe"
        let compiler = if isFSharp then "fsc.exe" else "csc.exe"
        sprintf "WebSharper %s compiler version %s" lang AssemblyVersionInformation.AssemblyFileVersion
        |> logger.Out
        if isFSharp then
            sprintf "(F# Compiler Service version %s)" AssemblyVersionInformation.FcsVersion
            |> logger.Out
        else
            sprintf "(Roslyn version %s)" AssemblyVersionInformation.RoslynVersion
            |> logger.Out
        logger.Out ""
        sprintf "Usage: %s [WebSharper options] [%s options]" exe compiler
        |> logger.Out
        logger.Out ""
        match helpKind with
        | NoHelp ->
            sprintf "WebSharper options help: %s --help" exe
            |> logger.Out
            sprintf "Unpack command help: %s unpack --help" exe
            |> logger.Out
            sprintf "Html command help: %s html --help" exe
            |> logger.Out
        | WSHelp ->
            sprintf """WebSharper options:
  --ws:<type>           Set WebSharper project type; one of:
                          library, site, bundle, bundleonly, html, extension
  --wig                 InterfaceGenerator project
                          same as --ws:extension
  --library             Library project
                          same as --ws:library
  --bundle              Single-page application project
                          same as --ws:bundle
  --bundleonly          SPA project with .js/.css outputs only
                          same as --ws:bundleonly
  --html                Static site generator project
                          same as --ws:html
  --site                Client-server application project
                          same as --ws:site
  --jsmap[+|-]          Enable source mapping
                          available for --ws:site,bundle,bundleonly
                          default: false
  --wswarnonly[+|-]     Print WebSharper-specific errors as warnings
                          default: false
  --dce[+|-]            Turn off dead code elimination for SPA projects
                          available for --ws:bundle,bundleonly
                          default: true
  --dlres[+|-]          Download remote js/css resources
                          available for --ws:site
                          default: false
  --printjs[+|-]        Print .js output
                          default: false
  --wsoutput:<dir>      Specify output directory for WebSharper-generated files
                          available for --ws:html,bundle,bundleOnly
  --jsoutput:<path>     Specify output file for compiled JavaScript
  --minjsoutput:<path>  Specify output file for compiled & minified JavaScript
  --project:<path>      Location of project file
  --closures[+|-]       Enable JS closure analysis
                          default: false
  --closures:movetotop  Enable JS closure optimization
  --standalone[+|-]     Use WebSharper compilation outside the service
                          default: false"""
            |> logger.Out
        | UnpackHelp ->
            sprintf "%s" (UnpackCommand.Instance.Usage.Replace("WebSharper.exe", exe))    
            |> logger.Out
        | HtmlHelp ->
            sprintf "%s" (HtmlCommand.Instance.Usage.Replace("WebSharper.exe", exe))    
            |> logger.Out
        Some 0

    match List.ofArray argv with
    | [] -> printInfo logger NoHelp
    | [ "--help" ] -> printInfo logger WSHelp
    | [ "unpack"; "--help" ] -> printInfo logger UnpackHelp
    | [ "html"; "--help" ] -> printInfo logger HtmlHelp
    | Cmd HtmlCommand.Instance r -> Some r
    | Cmd UnpackCommand.Instance r -> Some r
    | _ -> None

let RecognizeWebSharperArg a wsArgs =
    match a with
    | Flag "--jsmap" v -> Some { wsArgs with SourceMap = v }
    //| "--dts" -> Some { wsArgs with TypeScript = true } 
    | "--wig" -> Some { wsArgs with ProjectType = Some WIG }
    | "--bundle" -> Some { wsArgs with ProjectType =  Some Bundle }
    | "--bundleonly" -> Some { wsArgs with ProjectType = Some BundleOnly }
    | "--html" -> Some { wsArgs with ProjectType = Some Html }
    | "--site" -> Some { wsArgs with ProjectType = Some Website }
    | Flag "--wswarnonly" v -> Some { wsArgs with WarnOnly = v }
    | Flag "--dce" v -> Some { wsArgs with DeadCodeElimination = v }
    | StartsWith "--ws:" wsProjectType ->
        Some { wsArgs with ProjectType = ProjectType.Parse(wsProjectType) }
    | Flag "--dlres" v -> Some { wsArgs with DownloadResources = if v then Some true else None }
    | Flag "--printjs" v -> Some { wsArgs with PrintJS = v }
    | StartsWith "--wsoutput:" o ->
        Some { wsArgs with OutputDir = Some o }
    | StartsWith "--project:" p ->
        Some { wsArgs with ProjectFile = Path.Combine(Directory.GetCurrentDirectory(), p) }
    | StartsWith "--jsoutput:" f ->
        Some { wsArgs with JSOutputPath = Some f }
    | StartsWith "--minjsoutput:" f ->
        Some { wsArgs with MinJSOutputPath = Some f }
    | Flag "--closures" v ->
        Some { wsArgs with AnalyzeClosures = Some v }
    | StartsWith "--closures:" c ->
        match c.ToLower() with
        | "movetotop" ->
            Some { wsArgs with AnalyzeClosures = Some true }
        | _ ->
            printfn "--closures:%s argument unrecognized, must be one of: true, false, movetotop" c
            Some wsArgs
    | StartsWith "--scriptbaseurl" u -> Some { wsArgs with ScriptBaseUrl = Some u }
    | StartsWith "--wsconfig:" c ->
        if File.Exists c then
            Some (wsArgs.AddJson(File.ReadAllText c, Path.GetFileName c))
        else 
            argError (sprintf "Cannot find WebSharper configuration file %s" c)    
    | Flag "--standalone" v -> Some { wsArgs with Standalone = v }
    | _ -> 
        None

let SetDefaultProjectFile wsArgs isFSharp =
    let ext = if isFSharp then ".fsproj" else ".csproj"
    match wsArgs.ProjectFile with
    | null ->
        let projFiles = Directory.GetFiles(Directory.GetCurrentDirectory(), "*" + ext)
        match projFiles with
        | [| p |] -> { wsArgs with ProjectFile = p }
        | [| |] -> argError "Cannot find project file, specify argument --project"
        | _ -> argError "Multiple project files in folder, specify argument --project"
    | _ -> wsArgs

let SetScriptBaseUrl wsArgs =
    match wsArgs.ProjectType, wsArgs.ScriptBaseUrl with
    | Some (Bundle | BundleOnly), None -> { wsArgs with ScriptBaseUrl = Some "/Content/" }
    | Some Html, None -> { wsArgs with ScriptBaseUrl = Some "/Scripts/" }
    | _ -> wsArgs
