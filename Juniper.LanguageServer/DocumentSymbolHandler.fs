namespace Juniper.LanguageServer

open System
open OmniSharp.Extensions.LanguageServer.Protocol.Models
open OmniSharp.Extensions.LanguageServer.Protocol.Document
open OmniSharp.Extensions.LanguageServer.Protocol.Client.Capabilities
open Buffers



type DocumentSymbolHandler(bufferManager: BufferManager) =

    let pos (p: FParsec.Position) = Position((p.Line |> int) - 1, (p.Column |> int) - 1)

    let symbol name kind detail pos1 pos2 =
        let symbol = DocumentSymbol()
        symbol.Name <- name
        symbol.Kind <- kind
        symbol.Detail <- detail
        symbol.Range <- Range(pos1, pos2)
        symbol.SelectionRange <- Range(pos1, pos2)
        symbol

    let rec whew = function
        | Ast.LetDec { varName = ((p1,p2),_); right = right } ->
            
            let symbol = DocumentSymbol()
            symbol.Kind <- SymbolKind.Variable
            symbol.Detail <- "helou"
            symbol.Range <- Range(pos p1, pos p2)
            Some symbol
        | _ -> None

    let exampleSymbols =
        [   symbol "module" SymbolKind.Key "The module keyword lets us define the name of a module" (Position(0,0)) (Position(0, "module".Length-1))
            symbol "example name" SymbolKind.Interface "The name of the module" (Position(0,"module".Length+1)) (Position(0, Int32.MaxValue))
        ]
    
    interface IDocumentSymbolHandler with
        member this.GetRegistrationOptions(capability: DocumentSymbolCapability, clientCapabilities: ClientCapabilities): DocumentSymbolRegistrationOptions = 
            let registrationOptions = DocumentSymbolRegistrationOptions()
            registrationOptions.DocumentSelector <- Dependencies.junDocumentSelector
            registrationOptions

        member this.Handle(request: DocumentSymbolParams, cancellationToken: Threading.CancellationToken): Threading.Tasks.Task<SymbolInformationOrDocumentSymbolContainer> = 
            let documentPath = request.TextDocument.Uri.ToString()
            let doc = bufferManager.GetBuffer(documentPath)
            (*
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
                
            | _ -> Threading.Tasks.Task.FromResult(SymbolInformationOrDocumentSymbolContainer())*)

            exampleSymbols
            |> List.map SymbolInformationOrDocumentSymbol.Create
            |> SymbolInformationOrDocumentSymbolContainer.From
            |> Threading.Tasks.Task.FromResult
