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

open System
open System.IO.Pipes
open System.Threading
open System.IO
open System.Runtime.Serialization.Formatters.Binary
open FSharp.Compiler.CodeAnalysis
open System.Runtime.Caching
open WebSharper.Compiler.WsFscServiceCommon
open WebSharper.Compiler
open NLog.FSharp
open WebSharper.Compiler.FSharp.Compile
open WebSharper.Compiler.FSharp.ErrorPrinting
open WebSharper.Compiler.CommandTools

let startListening() =
    let nLogger = Logger()
    nLogger.Trace "Trace level is on"
    nLogger.Debug "Debug level is on"
    let checker = FSharpChecker.Create(keepAssemblyContents = true)
    let checkerFactory() = checker

    nLogger.Debug "Initializing memory cache"
    let memCache = MemoryCache.Default
    let tryGetMetadata (r: WebSharper.Compiler.FrontEnd.Assembly) =
        match r.LoadPath with
        // memCache.[<non-existent key>] won't error. It's returning null, if the key is not present.
        | Some x when memCache.[x] = null -> 
            match WebSharper.Compiler.FrontEnd.TryReadFromAssembly WebSharper.Compiler.FrontEnd.ReadOptions.FullMetadata r with
            | None ->
                None
            | result ->
                let policy = CacheItemPolicy()
                let monitor = new HostFileChangeMonitor([| x |])
                policy.ChangeMonitors.Add monitor
                memCache.Set(x, result, policy)
                nLogger.Trace "Storing assembly: %s" x
                memCache.[x] :?> Result<WebSharper.Core.Metadata.Info, string> option
        | Some x ->
            nLogger.Trace "Reading assembly: %s" x
            memCache.[x] :?> Result<WebSharper.Core.Metadata.Info, string> option
        | None ->
            // in-memory assembly may have no path. nLogger. 
            // nLogger.Trace "Reading assembly: %s" x makes this compilation fail. No Tracing here.
            WebSharper.Compiler.FrontEnd.TryReadFromAssembly WebSharper.Compiler.FrontEnd.ReadOptions.FullMetadata r

    let agent = MailboxProcessor.Start(fun inbox ->

        // the message processing function
        // compilations are serialed by a MailboxProcessor
        let rec messageLoop () = async {

            // a compilation failing because a client disconnects can still process the next compilation
            try
                // read a message
                let! (deserializedMessage: ArgsType, serverPipe: NamedPipeServerStream, token) = inbox.Receive()
                let tryGetDirectoryName (path: string) =
                    try
                       System.IO.Path.GetDirectoryName path |> Some
                    with
                    | :? System.DivideByZeroException -> None

                let joinTailIfSome (array: string array) =
                    match array with
                    | [||] -> None
                    | x -> System.String.Join(":", x) |> Some

                // read the project file
                let projectOption = 
                    deserializedMessage.args
                    |> Array.tryFind (fun x -> x.IndexOf("--project", System.StringComparison.OrdinalIgnoreCase) >= 0)
                    |> Option.bind (fun x -> x.Split(':') |> Array.tail |> joinTailIfSome)
                let projectDirOption = projectOption |> Option.bind tryGetDirectoryName
                match projectDirOption with
                | Some project -> 
                    System.Environment.CurrentDirectory <- project
                    nLogger.Debug "Compiling %s" projectOption.Value
                    let send paramPrint str = async {
                        let newMessage = paramPrint str
                        nLogger.Trace "Server sends: %s" newMessage
                        let bytes = System.Text.Encoding.UTF8.GetBytes(newMessage)
                        do! serverPipe.WriteAsync(bytes, 0, bytes.Length, token) |> Async.AwaitTask
                        serverPipe.Flush()
                    }
                    // all compilation output goes through a logger. This in standalone mode goes to stdout and stderr
                    // in service compilation it's proxied through a NamedPipeStream. prefixed with n: or e: or x: (error code)
                    let logger = { new LoggerBase() with
                            override _.Out s =
                                let sendOut = sprintf "n: %s" |> send
                                let asyncValue = 
                                    s
                                    |> sendOut
                                Async.RunSynchronously(asyncValue, cancellationToken = token)
                            override _.Error s =
                                let sendErr = sprintf "e: %s" |> send
                                let asyncValue = 
                                    s
                                    |> sendErr
                                Async.RunSynchronously(asyncValue, cancellationToken = token)
                        }
                            
                    let sendFinished = sprintf "x: %i" |> send
                    // use the same differentiation like --standalone

                    let compilationResultForDebugOrRelease() =
                        let parsedOptions = ParseOptions deserializedMessage.args logger
                        match parsedOptions with
                        | HelpOrCommand r ->
                            r // unexpected, wsfsc.exe should handle this
                        | ParsedOptions (wsConfig, warnSettings) ->
#if DEBUG
                            Compile wsConfig warnSettings logger checkerFactory tryGetMetadata
#else
                            try Compile wsConfig warnSettings logger checkerFactory tryGetMetadata
                            with 
                            | ArgumentError msg -> 
                                PrintGlobalError logger (msg + " - args: " + (deserializedMessage.args |> String.concat " "))
                                1
                            | e -> 
                                PrintGlobalError logger (sprintf "Global error: %A" e)
                                1
#endif
                    let returnValue = compilationResultForDebugOrRelease()
                    do! sendFinished returnValue
                | None ->
                    ()
            with 
            | ex -> 
                nLogger.ErrorException ex "Error in MailBoxProcessor loop"
            // loop to top
            return! messageLoop ()
            }

        // start the loop
        messageLoop ()
        )


    // start the reading/processing loop from that NamedPipeStream
    let handOverPipe (serverPipe: NamedPipeServerStream) (token: CancellationToken) =
        async {
            try
                let handleMessage (message: byte array) = 
                    async {
                        let ms = new MemoryStream(message)
                        ms.Position <- 0L
                        let bf = new BinaryFormatter()
                        let deserializedMessage: ArgsType = bf.Deserialize(ms) :?> ArgsType
                        agent.Post (deserializedMessage, serverPipe, token)
                        return None
                    }
                // collecting a full message in a ResizableBuffer. When it arrives do the "handleMessage" function on that.
                let! _ = readingMessages serverPipe handleMessage
                nLogger.Debug "Client has disconnected"
                serverPipe.Close()
            with
            | ex ->
                nLogger.ErrorException ex "Error in handleMessage loop"
            }

    let location = System.IO.Path.GetDirectoryName(System.Reflection.Assembly.GetEntryAssembly().Location)
    let pipeName = (location, "WsFscServicePipe") |> System.IO.Path.Combine |> hashPath
    // start listening. When Client connects, spawn a message processor and start another listen
    let rec pipeListener token = async {
        let serverPipe = new NamedPipeServerStream( 
                          pipeName, // name of the pipe,
                          PipeDirection.InOut, // diretcion of the pipe 
                          -1, // max number of server instances
                          PipeTransmissionMode.Message, // Transmissione Mode
                          PipeOptions.WriteThrough // the operation will not return the control until the write is completed
                          ||| PipeOptions.Asynchronous)
        do! serverPipe.WaitForConnectionAsync(token) |> Async.AwaitTask
        nLogger.Debug "Client connected on %s pipeName" pipeName
        Async.Start (handOverPipe serverPipe token, token)
        do! pipeListener token
        }

    let tokenSource = new CancellationTokenSource()
    nLogger.Debug "Server listening started on %s pipeName" pipeName
    Async.Start (pipeListener tokenSource.Token)
    // client starts the service without window. You have to shut down the service from Task Manager/ kill command.
    Console.ReadLine() |> ignore
    tokenSource.Cancel()


[<EntryPoint>]
let main _ =
    // One service should serve all compilations in a folder. Protect it with a global Mutex.
    let location = System.Reflection.Assembly.GetEntryAssembly().Location
    let mutexName =
        (location, "WsFscServiceMutex")
        |> System.IO.Path.Combine
        |> hashPath
        |> fun x -> "Global\\" + x
    let mutable mutex = null
    let mutexExists = Mutex.TryOpenExisting(mutexName, &mutex)
    if mutexExists then
        exit(1)
    mutex <- new Mutex(false, mutexName)
    mutex.WaitOne() |> ignore // should always instantly continue
    System.Runtime.GCSettings.LatencyMode <- System.Runtime.GCLatencyMode.Batch
    try
        startListening()
    // killing the task from Task Manager on Windows 10 will dispose the Mutex
    finally 
        try
            mutex.ReleaseMutex()
        finally
            try
                mutex.Close()
            finally
                mutex.Dispose()
    0 // exit code