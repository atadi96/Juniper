namespace Juniper.LanguageServer

open System
open OmniSharp.Extensions.LanguageServer.Protocol.Server
open Buffers
open Dependencies
open OmniSharp.Extensions.LanguageServer.Protocol.Server.Capabilities
open OmniSharp.Extensions.LanguageServer.Protocol
open OmniSharp.Extensions.LanguageServer.Protocol.Models
open OmniSharp.Extensions.LanguageServer.Protocol.Document
open OmniSharp.Extensions.LanguageServer.Protocol.Client.Capabilities



module RegistrationOptions =
    let setDocumentSelector<'t when 't :> ITextDocumentRegistrationOptions> (documentSelector: DocumentSelector) (options: 't) =
        options.DocumentSelector <- documentSelector
        options

type TextDocumentSyncHandler(router: ILanguageServerFacade, bufferManager: BufferManager, standardLibraryModules: IStandardLibraryModules, rootFolderService: IRootFolderService) =
    let documentSelector = Dependencies.junDocumentSelector

    let change = TextDocumentSyncKind.Full

    let parseRootFolder (currentFile: DocumentUri) =
        match rootFolderService.GetRootFolder() with
        | None -> []
        | Some rootFolder ->
            let rootFolderPath = rootFolder.ToUri().AbsolutePath
            let filePath = currentFile.ToUri().AbsolutePath

            if filePath.StartsWith(rootFolderPath) then

                System.IO.Directory.GetFiles(rootFolderPath, "*.jun", IO.SearchOption.AllDirectories)
                |> Seq.map (fun path -> Uri(Uri("file://"), path))
                |> Seq.filter (fun uri -> uri <> currentFile.ToUri())
                |> Seq.map (fun uri ->
                    let fileName = uri.AbsolutePath

                    uri.ToString()
                    |> bufferManager.GetBuffer
                    |> Option.map(fun buffer -> buffer.text)
                    |> Option.defaultWith (fun () ->
                        fileName
                        |> System.IO.File.ReadAllText
                    )
                    |> fun file -> uri.ToString(), file
                    |> JuniperCompiler.syntaxCheck
                    |> function
                        | Ok ast -> Some (fileName, ast)
                        | Error _ -> None
                )
                |> Seq.choose id
                |> List.ofSeq
            else
                []
        

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
                let otherFiles =
                    uri
                    |> parseRootFolder
                match ((documentPath, ast) :: otherFiles) |> JuniperCompiler.typeCheck (standardLibraryModules.GetStandardLibraryModules()) with
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
                    TypeError (ast, errMsg.errorData), diags
                | Error (JuniperCompiler.ErrorText errText) ->
                    let diag: Diagnostic = Diagnostic()
                    diag.Message <- errText
                    diag.Range <- Range(0, 0, 0, 1)
                    diag.Severity <- DiagnosticSeverity.Error
                    TypeError (ast, None), [ diag ]
                    
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
            |> fun options ->
                options.IncludeText <- true
                options

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
            
            bufferManager.RemoveBuffer(request.TextDocument.Uri.ToString())
            MediatR.Unit.Task

        member this.Handle(request: DidSaveTextDocumentParams, cancellationToken: Threading.CancellationToken): Threading.Tasks.Task<MediatR.Unit> = 

            handle (request.TextDocument.Uri, Nullable<_>(), request.Text)
