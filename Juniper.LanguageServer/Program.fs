open System
open Microsoft.Language.Xml
open OmniSharp.Extensions.LanguageServer.Protocol.Server
open OmniSharp.Extensions.LanguageServer.Protocol.Document
open OmniSharp.Extensions.LanguageServer.Protocol.Models
open OmniSharp.Extensions.LanguageServer.Protocol.Client.Capabilities
open OmniSharp.Extensions.LanguageServer.Protocol
open OmniSharp.Extensions.LanguageServer.Protocol.Server.Capabilities

type IStandardLibraryModules =
    abstract GetStandardLibraryModules : unit -> (string * Ast.Module) list

type Buffer =
    {
        text: string
        lastAst: (Ast.Module * string) option
        lastTypeCheck: (JuniperCompiler.TypeCheckedProgram * string) option
        compilation: Compilation
    }
and Compilation =
    | SyntaxError
    | TypeError of Ast.Module
    | Compiled of Ast.Module * JuniperCompiler.TypeCheckedProgram

module Buffer =
    let create text compilation =
        let (ast, typeCheck) =
            match compilation with
            | SyntaxError -> None, None
            | TypeError ast -> Some (ast, text), None
            | Compiled (ast, typeCheck) -> Some (ast, text), Some (typeCheck, text)
        {
            text = text
            lastAst = ast
            lastTypeCheck = typeCheck
            compilation = compilation
        }

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

type TextDocumentSyncHandler(router: ILanguageServerFacade, bufferManager: BufferManager, standardLibraryModules: IStandardLibraryModules) =
    let documentSelector =
        let docuFilter = DocumentFilter()
        docuFilter.Pattern <- "**/*.jun"
        DocumentSelector(docuFilter)

    let change = TextDocumentSyncKind.Full

    let handle (uri: DocumentUri, version: Nullable<int>, text: string) =
        let documentPath = uri.ToString()

        let (compilation, diagnostics) =
            let fixPos (pos: FParsec.Position): Position =
                let line = pos.Line |> int
                let col = pos.Column |> int
                Position(line - 1, col)
            let fixPos' (pos: FParsec.Position): Position =
                let line = pos.Line |> int
                let col = pos.Column |> int
                Position(line - 1, col - 1)

            match (documentPath, text) |> JuniperCompiler.syntaxCheck with
            | Error (startPosition, endPosition, message) ->
                let endPosition = endPosition |> Option.defaultValue startPosition
                let diag: Diagnostic = Diagnostic()
                diag.Message <- message
                diag.Range <- Range(startPosition |> fixPos, endPosition |> fixPos)
                diag.Severity <- DiagnosticSeverity.Error
                SyntaxError, [ diag ]
                
            | Ok ast ->
                match (documentPath, ast) |> JuniperCompiler.typeCheck (standardLibraryModules.GetStandardLibraryModules()) with
                | Ok typeCheck -> Compiled (ast, typeCheck), [ ]
                | Error (JuniperCompiler.ErrorMessage errMsg) ->
                    let diags =
                        errMsg.positions
                        |> List.map (fun (startPosition, endPosition) ->
                            let diag = Diagnostic()
                            diag.Message <- errMsg.message.Force()
                            diag.Range <- Range(startPosition |> fixPos', endPosition |> fixPos')
                            diag.Severity <- DiagnosticSeverity.Error
                            diag
                        )
                    TypeError ast, diags
                | Error (JuniperCompiler.ErrorText errText) ->
                    let diag: Diagnostic = Diagnostic()
                    diag.Message <- errText
                    diag.Range <- Range(0, 0, 0, 1)
                    diag.Severity <- DiagnosticSeverity.Error
                    TypeError ast, [ diag ]
                    
        bufferManager.UpdateBuffer(documentPath, Buffer.create text compilation) |> ignore

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

type DocumentSymbolHandler(bufferManager: BufferManager) =
    let rec whew = function
        | Ast.LetDec { varName = ((p1,p2),_); right = right } ->
            let pos (p: FParsec.Position) = Position((p.Line |> int) - 1, (p.Column |> int) - 1)
            let symbol = DocumentSymbol()
            symbol.Kind <- SymbolKind.Variable
            symbol.Detail <- "helou"
            symbol.Range <- Range(pos p1, pos p2)
            Some symbol
        | _ -> None
    
    interface IDocumentSymbolHandler with
        member this.GetRegistrationOptions(capability: DocumentSymbolCapability, clientCapabilities: ClientCapabilities): DocumentSymbolRegistrationOptions = 
            let registrationOptions = DocumentSymbolRegistrationOptions()
            registrationOptions.DocumentSelector <-
                let documentFilter = DocumentFilter()
                documentFilter.Pattern <- "**/*.jun"
                DocumentSelector(documentFilter)
            registrationOptions

        member this.Handle(request: DocumentSymbolParams, cancellationToken: Threading.CancellationToken): Threading.Tasks.Task<SymbolInformationOrDocumentSymbolContainer> = 
            let documentPath = request.TextDocument.Uri.ToString()
            let doc = bufferManager.GetBuffer(documentPath)
            match doc with
            | Some { lastAst = Some (Ast.Module declarationsPos, _) } ->
                let symbols =
                    declarationsPos
                    |> Seq.map snd
                    |> Seq.choose whew
                    |> Seq.map SymbolInformationOrDocumentSymbol.Create
                    |> Container.From
                SymbolInformationOrDocumentSymbolContainer.From(symbols)
                |> Threading.Tasks.Task.FromResult
                
            | _ -> Threading.Tasks.Task.FromResult(SymbolInformationOrDocumentSymbolContainer())

type StandardLibraryModules(stdLibDirectory: string) =
    let stdLibModules =
        StandardLibrary.modules
        |> Seq.map (sprintf "%s/%s.jun" stdLibDirectory)
        |> Seq.map (fun fileName ->
            fileName
            |> System.IO.File.ReadAllText
            |> fun file -> (fileName, file)
            |> JuniperCompiler.syntaxCheck
            |> function
                | Ok ast -> (fileName, ast)
                | _ -> failwith "Error: standard library does not compile!"
        )
        |> List.ofSeq

    interface IStandardLibraryModules with
        member this.GetStandardLibraryModules(): (string * Ast.Module) list = stdLibModules

open Microsoft.Extensions.Logging
open Microsoft.Extensions.DependencyInjection

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
