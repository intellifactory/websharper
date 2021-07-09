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

module WebSharper.FSharp.Program

open System.IO
open WebSharper.Compiler
module C = WebSharper.Compiler.Commands

open FSharp.Compiler.CodeAnalysis
open WebSharper.Compiler.FSharp.Compile
open WebSharper.FSharp.NamedPipeClient
open WebSharper.Compiler.CommandTools
open WebSharper.Compiler.FSharp.ErrorPrinting
open NLog.FSharp

let formatArgv (argv: string[]) =
    match argv with
    | [| a |] when a.StartsWith "@" ->
        File.ReadAllLines a.[1..]
    | [| f; a |] when (f.EndsWith "fsc.exe" || f.EndsWith "fsc.dll") && a.StartsWith "@" ->
        Array.append [| f |] (File.ReadAllLines a.[1..])
    | _ -> argv

[<EntryPoint>]
let main(argv) =
    System.Runtime.GCSettings.LatencyMode <- System.Runtime.GCLatencyMode.Batch
    let nLogger = Logger()
    nLogger.Trace "Trace level is on"
    nLogger.Debug "Debug level is on"
    let argv = formatArgv argv
    let logger = ConsoleLogger()   
    let parsedOptions = ParseOptions argv logger
    match parsedOptions with
    | HelpOrCommand r ->
        r
    | ParsedOptions (wsConfig, warnSettings) ->
        if wsConfig.Standalone then
            let reason =
                if System.Environment.GetEnvironmentVariables()
                    |> Seq.cast<System.Collections.DictionaryEntry>
                    |> Seq.exists (fun x -> (x.Key :?> string).ToLower() = "websharperbuildservice" && (x.Value :?> string).ToLower() = "false")
                then
                    "WebSharperBuildService environment variable is set to false"
                else
                    "--standalone compile flag is set or WebSharperStandalone targets variable set"
            nLogger.Debug "Start compilation in standalone mode because %s." reason
            let createChecker() = FSharpChecker.Create(keepAssemblyContents = true)
            let tryGetMetadata = WebSharper.Compiler.FrontEnd.TryReadFromAssembly WebSharper.Compiler.FrontEnd.ReadOptions.FullMetadata
#if DEBUG
            StandAloneCompile wsConfig warnSettings logger createChecker tryGetMetadata  
#else
            try StandAloneCompile wsConfig warnSettings logger createChecker tryGetMetadata
            with 
            | ArgumentError msg -> 
                PrintGlobalError logger (msg + " - args: " + (argv |> String.concat " "))
                1
            | e -> 
                PrintGlobalError logger (sprintf "Global error: %A" e)
                1
#endif
        else
            nLogger.Debug "Start compilation with wsfscservice"
            // The #if DEBUG ... #else behavior is implemented in the service.
            // NamedPipeService won't throw exception in the client.
            let serviceExitCode = sendCompileCommand argv
            if serviceExitCode = 0 then
                UnpackOrWIG wsConfig warnSettings logger    
            else
                serviceExitCode
