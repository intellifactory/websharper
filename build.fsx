#if INTERACTIVE
#r "nuget: FAKE.Core"
#r "nuget: Fake.Core.Target"
#r "nuget: Fake.IO.FileSystem"
#r "nuget: Fake.Tools.Git"
#r "nuget: Fake.DotNet.Cli"
#r "nuget: Fake.DotNet.AssemblyInfoFile"
#r "nuget: Fake.DotNet.Paket"
#r "nuget: Paket.Core"
#else
#r "paket:
nuget FAKE.Core
nuget Fake.Core.Target
nuget Fake.IO.FileSystem
nuget Fake.Tools.Git
nuget Fake.DotNet.Cli
nuget Fake.DotNet.AssemblyInfoFile
nuget Fake.DotNet.Paket
nuget Paket.Core //"
#endif

#load "paket-files/wsbuild/github.com/dotnet-websharper/build-script/WebSharper.Fake.fsx"

#r "System.Xml.Linq"

#if INTERACTIVE
#r "nuget: NUglify"
#else
#r "paket:
nuget NUglify //"
#endif

open System.IO
open System.Diagnostics
open System.Xml
open System.Xml.Linq
open System.Xml.XPath
open Fake.Core
open Fake.Core.TargetOperators
open Fake.DotNet
open Fake.IO
open Fake.IO.FileSystemOperators
open WebSharper.Fake
open System.Diagnostics

let version = "5.0"
let pre = Some "preview1"

let baseVersion =
    version + match pre with None -> "" | Some x -> "-" + x
    |> Paket.SemVer.Parse

let specificFw = Environment.environVarOrNone "WS_TARGET_FW"

let targets = MakeTargets {
    WSTargets.Default (fun () -> ComputeVersion (Some baseVersion)) with
        BuildAction =
            let buildSln sln =
                let sln =
                    match specificFw with
                    | None -> sln
                    | Some d -> d </> sln
                match Environment.environVarOrNone "OS" with
                | Some "Windows_NT" ->
                    BuildAction.Projects [sln]
                | _ ->
                    BuildAction.Custom <| fun mode ->
                        DotNet.build (fun p ->
                            { p with
                                Configuration = DotNet.BuildConfiguration.fromString (mode.ToString())
                                MSBuildParams = 
                                    { p.MSBuildParams with 
                                        DisableInternalBinLog = true // workaround for https://github.com/fsharp/FAKE/issues/2515
                                    }
                            }) sln
            let dest mode lang =
                __SOURCE_DIRECTORY__ </> "build" </> mode.ToString() </> lang
            let publishExe (mode: BuildMode) fw input output explicitlyCopyFsCore =
                let outputPath =
                    __SOURCE_DIRECTORY__ </> "build" </> mode.ToString() </> output </> fw </> "deploy"
                DotNet.publish (fun p ->
                    { p with
                        Framework = Some fw
                        OutputPath = Some outputPath
                        NoRestore = true
                        Configuration = DotNet.BuildConfiguration.fromString (mode.ToString())
                        MSBuildParams = 
                            { p.MSBuildParams with 
                                DisableInternalBinLog = true // workaround for https://github.com/fsharp/FAKE/issues/2515
                            }
                    }) input
                if explicitlyCopyFsCore then
                    let fsharpCoreLib = __SOURCE_DIRECTORY__ </> "packages/includes/FSharp.Core/lib/netstandard2.0"
                    [ 
                        fsharpCoreLib </> "FSharp.Core.dll" 
                    ] 
                    |> Shell.copy outputPath                
            BuildAction.Multiple [
                buildSln "WebSharper.Compiler.sln"
                BuildAction.Custom <| fun mode ->
                    publishExe mode "net5.0" "src/compiler/WebSharper.FSharp/WebSharper.FSharp.fsproj" "FSharp" true
                    publishExe mode "net5.0" "src/compiler/WebSharper.FSharp.Service/WebSharper.FSharp.Service.fsproj" "FSharp" true
                    publishExe mode "net5.0" "src/compiler/WebSharper.CSharp/WebSharper.CSharp.fsproj" "CSharp" true
                buildSln "WebSharper.sln"
            ]
}

let NeedsBuilding input output =
    let i = FileInfo(input)
    let o = FileInfo(output)
    not o.Exists || o.LastWriteTimeUtc < i.LastWriteTimeUtc

let Minify () =
    let minify (path: string) =
        let out = Path.ChangeExtension(path, ".min.js")
        if NeedsBuilding path out then
            let raw = File.ReadAllText(path)
            let mjs = NUglify.Uglify.Js(raw).Code
            File.WriteAllText(Path.ChangeExtension(path, ".min.js"), mjs)
            stdout.WriteLine("Written {0}", out)
    minify "src/compiler/WebSharper.Core.JavaScript/Runtime.js"
    minify "src/stdlib/WebSharper.Main/Json.js"
    minify "src/stdlib/WebSharper.Main/AnimFrame.js"

let MakeNetStandardTypesList() =
    let f = FileInfo("src/compiler/WebSharper.Core/netstandardtypes.txt")
    if not f.Exists then
        let asm =
            "packages/includes/NETStandard.Library.Ref/ref/netstandard2.1/netstandard.dll"
            |> Mono.Cecil.AssemblyDefinition.ReadAssembly
        use s = f.OpenWrite()
        use w = new StreamWriter(s)
        w.WriteLine(asm.FullName)
        let rec writeType (t: Mono.Cecil.TypeDefinition) =
            w.WriteLine(t.FullName.Replace('/', '+'))
            Seq.iter writeType t.NestedTypes
        Seq.iter writeType asm.MainModule.Types

let AddToolVersions() =
    let lockFile =
        Paket.LockFile.LoadFrom(__SOURCE_DIRECTORY__ </> "paket.lock")
    let roslynVersion = 
        lockFile
            .GetGroup(Paket.Domain.GroupName "main")
            .GetPackage(Paket.Domain.PackageName "Microsoft.CodeAnalysis.CSharp")
            .Version.AsString
    let fcsVersion = 
        lockFile
            .GetGroup(Paket.Domain.GroupName "fcs")
            .GetPackage(Paket.Domain.PackageName "FSharp.Compiler.Service")
            .Version.AsString
    let inFile = "build/AssemblyInfo.fs" // Generated by WS-GenAssemblyInfo
    let outFile = "msbuild/AssemblyInfo.fs"
    let t =
        String.concat "\r\n" [
            yield File.ReadAllText(inFile)
            yield sprintf "    let [<Literal>] FcsVersion = \"%s\"" fcsVersion
            yield sprintf "    let [<Literal>] RoslynVersion = \"%s\"" roslynVersion
        ]
    if not (File.Exists(outFile) && t = File.ReadAllText(outFile)) then
        File.WriteAllText(outFile, t)

Target.create "Prepare" <| fun _ ->
    Minify()
    MakeNetStandardTypesList()
    AddToolVersions()

Target.create "RunTestsRelease" <| fun _ ->
    Trace.log "Starting Web test project"
    let mutable startedOk = false
    let started = Event<unit>()

    use webTestsProc = new Process()
    webTestsProc.StartInfo.FileName <- @"build\Release\Tests\net5.0\Web.exe"
    webTestsProc.StartInfo.WorkingDirectory <- @"tests\Web"
    webTestsProc.StartInfo.UseShellExecute <- false
    webTestsProc.StartInfo.RedirectStandardOutput <- true
    
    webTestsProc.OutputDataReceived.Add(fun d -> 
        if not (isNull d) then
            Trace.log d.Data
            if d.Data.Contains("Application started.") then
                startedOk <- true   
                started.Trigger()
    )
    webTestsProc.Exited.Add(fun _ -> 
        if not startedOk then
            failwith "Starting Web test project failed"    
    )

    webTestsProc.Start()
    webTestsProc.BeginOutputReadLine()
    started.Publish |> Async.AwaitEvent |> Async.RunSynchronously

    let res =
        Shell.Exec(
            "packages/test/Chutzpah/tools/chutzpah.console.exe", "http://localhost:5000/consoletests /engine Chrome /silent"
        )
    webTestsProc.Kill()
    if res <> 0 then
        failwith "Chutzpah test run failed"

targets.AddPrebuild "Prepare"

Target.create "Stop" <| fun _ ->
    try
        Process.GetProcessesByName("wsfscservice")
        |> Array.iter (fun x -> x.Kill())
        |> ignore
    with
    | _ -> ()

"Stop" ==> "WS-Clean"
"Stop" ==> "WS-Restore"

"WS-BuildRelease"
    ==> "RunTestsRelease"
    ?=> "WS-Package"

"RunTestsRelease"
    ==> "CI-Release"
    
Target.runOrDefaultWithArguments "Build"
