open System
open Microsoft.Language.Xml
open OmniSharp.Extensions.LanguageServer.Protocol.Server
open OmniSharp.Extensions.LanguageServer.Protocol.Document
open OmniSharp.Extensions.LanguageServer.Protocol.Models
open OmniSharp.Extensions.LanguageServer.Protocol.Client.Capabilities
open OmniSharp.Extensions.LanguageServer.Protocol
open OmniSharp.Extensions.LanguageServer.Protocol.Server.Capabilities


// For more information see https://aka.ms/fsharp-console-apps
type BufferManager() =
    let buffers = System.Collections.Concurrent.ConcurrentDictionary<string,Buffer>()

    member __.UpdateBuffer(documentPath: string, buffer: Buffer) =
        buffers.AddOrUpdate(documentPath, buffer, fun _ _ -> buffer)

    member __.GetBuffer(documentPath: string) =
        match documentPath |> buffers.TryGetValue with
        | true, buffer -> Some buffer
        | _ -> None

module RegistrationOptions =
    let setDocumentSelector<'t when 't :> ITextDocumentRegistrationOptions> (documentSelector: DocumentSelector) (options: 't) =
        options.DocumentSelector <- documentSelector
        options

type TextDocumentSyncHandler(router: ILanguageServerFacade, bufferManager: BufferManager) =
    let documentSelector =
        let docuFilter = DocumentFilter()
        docuFilter.Pattern <- "**/*.jun"
        DocumentSelector(docuFilter)

    let change = TextDocumentSyncKind.Full

    let handle (uri: DocumentUri, version: Nullable<int>, text: string) =
        let documentPath = uri.ToString()
        
        bufferManager.UpdateBuffer(documentPath, StringBuffer(text)) |> ignore

        let diagnostics =
            let fixPos (pos: FParsec.Position): Position =
                let line = pos.Line |> int
                let col = pos.Column |> int
                Position(line - 1, col)

            match (documentPath, text) |> JuniperCompiler.syntaxCheck with
            | Error (startPosition, endPosition, message) ->
                let endPosition = endPosition |> Option.defaultValue startPosition
                let diag: Diagnostic = Diagnostic()
                diag.Message <- message
                diag.Range <- Range(startPosition |> fixPos, endPosition |> fixPos)
                diag.Severity <- DiagnosticSeverity.Error
                [ diag ]
                
            | Ok ast ->
                match (documentPath, ast) |> JuniperCompiler.typeCheck with
                | Ok _ -> [ ]
                | Error (JuniperCompiler.ErrorMessage errMsg) ->
                    errMsg.positions
                    |> List.map (fun (startPosition, endPosition) ->
                        let diag = Diagnostic()
                        diag.Message <- errMsg.errStr.Force()
                        diag.Range <- Range(startPosition |> fixPos, endPosition |> fixPos)
                        diag.Severity <- DiagnosticSeverity.Error
                        diag
                    )
                | Error (JuniperCompiler.ErrorText errText) ->
                    let diag: Diagnostic = Diagnostic()
                    diag.Message <- errText
                    diag.Range <- Range(0, 0, 0, 1)
                    diag.Severity <- DiagnosticSeverity.Error
                    [ diag ]

        let diagnosticParams = PublishDiagnosticsParams()
        diagnosticParams.Uri <- uri
        diagnosticParams.Version <- version
        diagnosticParams.Diagnostics <- Container.From(diagnostics)

        router.TextDocument.PublishDiagnostics(diagnosticParams)
        
        //router.Window.LogInfo($"Updated buffer for document: {documentPath}\n{text}");

        MediatR.Unit.Task
    interface ITextDocumentSyncHandler with
        member this.GetRegistrationOptions(capability: SynchronizationCapability, clientCapabilities: ClientCapabilities): TextDocumentChangeRegistrationOptions = 
            let options = TextDocumentChangeRegistrationOptions()
            options.DocumentSelector <- documentSelector
            options.SyncKind <- change
            options

        member this.GetRegistrationOptions(capability: SynchronizationCapability, clientCapabilities: ClientCapabilities): TextDocumentOpenRegistrationOptions =
            TextDocumentOpenRegistrationOptions()
            |> RegistrationOptions.setDocumentSelector documentSelector

        member this.GetRegistrationOptions(capability: SynchronizationCapability, clientCapabilities: ClientCapabilities): TextDocumentCloseRegistrationOptions = 
            TextDocumentCloseRegistrationOptions()
            |> RegistrationOptions.setDocumentSelector documentSelector

        member this.GetRegistrationOptions(capability: SynchronizationCapability, clientCapabilities: ClientCapabilities): TextDocumentSaveRegistrationOptions =
            TextDocumentSaveRegistrationOptions()
            |> RegistrationOptions.setDocumentSelector documentSelector

        member this.GetTextDocumentAttributes(uri: DocumentUri): TextDocumentAttributes =
            TextDocumentAttributes(uri, "juniper")

        member this.Handle(request: DidChangeTextDocumentParams, cancellationToken: Threading.CancellationToken): Threading.Tasks.Task<MediatR.Unit> = 
            let text =
                request.ContentChanges
                |> Seq.tryHead
                |> Option.map (fun change -> change.Text)
                |> Option.defaultValue (Unchecked.defaultof<_>)

            handle(request.TextDocument.Uri, request.TextDocument.Version, text)

        member this.Handle(request: DidOpenTextDocumentParams, cancellationToken: Threading.CancellationToken): Threading.Tasks.Task<MediatR.Unit> = 
            
            handle (request.TextDocument.Uri, request.TextDocument.Version, request.TextDocument.Text)

        member this.Handle(request: DidCloseTextDocumentParams, cancellationToken: Threading.CancellationToken): Threading.Tasks.Task<MediatR.Unit> = 
            
            handle (request.TextDocument.Uri, Nullable<_>(), "")

        member this.Handle(request: DidSaveTextDocumentParams, cancellationToken: Threading.CancellationToken): Threading.Tasks.Task<MediatR.Unit> = 

            handle (request.TextDocument.Uri, Nullable<_>(), request.Text)

open Microsoft.Extensions.Logging
open Microsoft.Extensions.DependencyInjection

let configureServices (services: IServiceCollection) =
    services
        .AddSingleton<BufferManager>()
        .AddSingleton<TextDocumentSyncHandler>()
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
