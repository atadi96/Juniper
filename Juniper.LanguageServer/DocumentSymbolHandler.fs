namespace Juniper.LanguageServer

open System
open OmniSharp.Extensions.LanguageServer.Protocol.Models
open OmniSharp.Extensions.LanguageServer.Protocol.Document
open OmniSharp.Extensions.LanguageServer.Protocol.Client.Capabilities
open Buffers



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
            registrationOptions.DocumentSelector <- Dependencies.junDocumentSelector
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
