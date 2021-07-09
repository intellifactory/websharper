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

namespace WebSharper.Sitelets.Offline

open System
open System.IO
open System.Reflection
open WebSharper.Core
open WebSharper.Compiler
open WebSharper.Sitelets
module C = WebSharper.Compiler.Commands
module H = WebSharper.Compiler.HtmlCommand
module D = WebSharper.Compiler.DownloadResources
module PC = WebSharper.PathConventions

/// Implements the WebSharper executable plugin for generating
/// offline pages with sitelets. To use, run `WebSharper.exe sitelets`.
[<Sealed>]
type HtmlCommand() =
    interface H.IHtmlCommand with
        member this.Execute(env, options) =
            let aR = AssemblyResolver.Create()
            Directory.CreateDirectory options.OutputDirectory |> ignore
            // process extra.files
            Extra.CopyFiles
                options.ProjectDirectory
                options.OutputDirectory
            let scriptDir =
                System.IO.Path.Combine(options.OutputDirectory, "Scripts")
                |> Directory.CreateDirectory
            let aR =
                let aR = aR.SearchPaths(options.ReferenceAssemblyPaths)
                options.ReferenceAssemblyPaths
                |> Seq.map Path.GetDirectoryName
                |> Seq.append [Path.GetDirectoryName(options.MainAssemblyPath)]
                |> aR.SearchDirectories
            aR.Wrap <| fun () ->
                // Load the sitelet
                let loadSite (file: string) =
                    let assemblyName = AssemblyName.GetAssemblyName(file)
                    let assembly = aR.Resolve(assemblyName)
                    match assembly with
                    | None ->
                        failwithf "Failed to load %s" file
                    | Some assembly ->
                        let siteletType =
                            assembly.CustomAttributes |> Seq.tryPick (fun a ->
                                if a.AttributeType.FullName = "WebSharper.Sitelets.WebsiteAttribute" then
                                    let args = a.ConstructorArguments
                                    if args.Count = 1 then 
                                        Some a.ConstructorArguments.[0]
                                    else
                                        None
                                else
                                    None
                            )
                        let aT = typeof<WebsiteAttribute>
                        match siteletType with
                        | Some a ->
                            let typeName = (a.Value :?> Type).AssemblyQualifiedName
                            let ty = System.Type.GetType(typeName, true)
                            
                            let website = Activator.CreateInstance(ty)
                            let innerTypeOpt =
                                ty.GetInterfaces()
                                |> Seq.tryPick (fun iT ->
                                    if iT.FullName.StartsWith("WebSharper.Sitelets.IWebsite`1") then
                                        Some (iT.GetGenericArguments().[0])
                                    else
                                        None)
                            let innerType =
                                match innerTypeOpt with
                                | Some t -> t
                                | None -> failwith "Type is not implementing IWebsite"

                            WebSharper.Sitelets.Utils.GetSitelet innerType website
                let (sitelet, actions) = loadSite options.MainAssemblyPath

                if options.DownloadResources then
                    let assemblies = [options.MainAssemblyPath] @ options.ReferenceAssemblyPaths
                    for p in assemblies do
                        D.DownloadResource p options.OutputDirectory

                // Write site content.
                Output.WriteSite aR {
                    Sitelet = sitelet
                    Options = options
                    Actions = actions
                    UnpackSourceMap = options.UnpackSourceMap
                    UnpackTypeScript = options.UnpackTypeScript
                }
                |> Async.RunSynchronously
                C.Ok
