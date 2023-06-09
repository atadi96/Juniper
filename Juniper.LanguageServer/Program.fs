open System
open Microsoft.Extensions.Logging
open Microsoft.Extensions.DependencyInjection
open Juniper.LanguageServer.Buffers
open Juniper.LanguageServer
open Juniper.LanguageServer.Dependencies
open Juniper.LanguageServer.DependencyImplementations
open OmniSharp.Extensions.LanguageServer.Server

let configureServices (services: IServiceCollection) =
    let rootFolderHolder = RootFolderHolder()
    services
        .AddSingleton<BufferManager>()
        .AddSingleton<TextDocumentSyncHandler>()
        .AddSingleton<IStandardLibraryModules>(StandardLibraryModules(@"C:\Users\atadi\source\repos\Juniper\juniper-repo\Juniper\junstd"))
        .AddSingleton<IRootFolderSetter>(rootFolderHolder)
        .AddSingleton<IRootFolderService>(rootFolderHolder)
    |> ignore

let configureServer (options: OmniSharp.Extensions.LanguageServer.Server.LanguageServerOptions): unit =
    options
        .WithInput(Console.OpenStandardInput())
        .WithOutput(Console.OpenStandardOutput())
        .WithLoggerFactory(new LoggerFactory())
        //.AddDefaultLoggintProvider()
        //.WithMinimumLogLevel()
        .WithServices(configureServices)
        .WithHandler<TextDocumentSyncHandler>()
        //.WithHandler<DocumentSymbolHandler>()
        .WithHandler<CompletionHandler>()
        .WithHandler<SemanticTokenHandler>()
        .OnInitialize(fun server request token ->
            let () =
                request.WorkspaceFolders
                |> Option.ofObj
                |> Option.bind (fun workspaceFolderCollection -> workspaceFolderCollection |> Seq.tryHead)
                |> Option.map (fun workspaceFolder -> workspaceFolder.Uri)
                |> Option.orElse (
                    request.RootUri
                    |> Option.ofObj
                )
                |> Option.iter (server.Services.GetRequiredService<IRootFolderSetter>().SetRootFolder)

            System.Threading.Tasks.Task.CompletedTask
        )

    |> ignore


let runServer =
    System.Diagnostics.Debugger.Launch() |> ignore
    while not (System.Diagnostics.Debugger.IsAttached) do
        System.Threading.Thread.Sleep(100)

    task {
        let! server =
            OmniSharp.Extensions.LanguageServer.Server.LanguageServer.From(configureServer)

        return! server.WaitForExit
    }

async {
    do! Async.SwitchToThreadPool()
    do! runServer |> Async.AwaitTask
}
|> Async.RunSynchronously
