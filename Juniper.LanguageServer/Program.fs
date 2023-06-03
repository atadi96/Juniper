open System
open Microsoft.Extensions.Logging
open Microsoft.Extensions.DependencyInjection
open Juniper.LanguageServer.Buffers
open Juniper.LanguageServer
open Juniper.LanguageServer.Dependencies
open Juniper.LanguageServer.DependencyImplementations

let configureServices (services: IServiceCollection) =
    services
        .AddSingleton<BufferManager>()
        .AddSingleton<TextDocumentSyncHandler>()
        .AddSingleton<IStandardLibraryModules>(StandardLibraryModules(@"C:\Users\atadi\source\repos\Juniper\juniper-repo\Juniper\junstd"))
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
        .WithHandler<DocumentSymbolHandler>()
        .WithHandler<CompletionHandler>()

    |> ignore


let runServer =
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
