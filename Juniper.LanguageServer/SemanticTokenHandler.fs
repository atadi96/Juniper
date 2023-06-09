namespace Juniper.LanguageServer

open System
open Buffers
open Dependencies
open OmniSharp.Extensions.LanguageServer.Protocol.Document
open OmniSharp.Extensions.LanguageServer.Protocol.Client.Capabilities
open OmniSharp.Extensions.LanguageServer.Protocol.Models

type SemanticToken =
    {
        range: Range
        tokenType: SemanticTokenType
        tokenModifiers: SemanticTokenModifier list
    }

open Ast

module SemanticToken =
(*
    let private fromDeclaration (declaration: Declaration) =
        match declaration with
        | FunctionDec {name = ((startPos, endPos),name)} ->
            let nameToken =
                {
                    line = (startPos.Line |> int) - 1
                    startChar = (startPos.Column |> int) - 1
                    length = (endPos.Column - startPos.Column) |> int
                    tokenType = SemanticTokenType.Interface
                    tokenModifiers = []
                }
            [ nameToken ]
        | _ -> []

    let fromAst (Module declarations) =
        declarations
        |> Seq.collect (fun (_pos, decl) -> fromDeclaration decl)
        *)
    let fromTokens (tokens: seq<Range * Token>) =
        tokens
        |> Seq.map (fun (range, token) ->
            let (tokenType, modifiers) =
                match token with
                | KeywordToken _ -> SemanticTokenType.Keyword, []
                | VariableToken _ -> SemanticTokenType.Variable, []
                | FunctionNameToken _ -> SemanticTokenType.Function, []
                | TypeParameterToken _ -> SemanticTokenType.TypeParameter, []
                | TypeToken -> SemanticTokenType.Type, []
                | InlineCodeToken  _ -> SemanticTokenType.Macro, []
                | ModuleNameToken  _ -> SemanticTokenType.Namespace, []
                | ModuleMemberToken _ -> SemanticTokenType.Property, [SemanticTokenModifier.Static]
                | ValueConstructorToken _ -> SemanticTokenType.EnumMember, []
                | IncludeToken _ -> SemanticTokenType.Macro, []
                | BinaryOpToken _ -> SemanticTokenType.Operator, []
                | UnaryOpToken _ -> SemanticTokenType.Operator, []
                | UnionTypeNameToken _ -> SemanticTokenType.Enum, []
                | RecordFieldNameToken _ -> SemanticTokenType.Property, []
                | UnitToken -> SemanticTokenType.Type, []
                | IntegerToken _ -> SemanticTokenType.Number, []
                | FloatToken _ -> SemanticTokenType.Number, []
                | TextToken _ -> SemanticTokenType.String, []
            {
                range = range
                tokenType = tokenType
                tokenModifiers = modifiers
            }
        )

type SemanticTokenHandler(bufferManager: BufferManager) =
    let registrationOption =
        let registrationOption = SemanticTokensRegistrationOptions()

        registrationOption.DocumentSelector <- junDocumentSelector

        registrationOption.Full <-
            let full = SemanticTokensCapabilityRequestFull()
            full.Delta <- false
            BooleanOr(full)

        registrationOption.Legend <-
            let legend = SemanticTokensLegend()
            legend.TokenTypes <- SemanticTokenType.Defaults |> Container.From
            legend.TokenModifiers <- SemanticTokenModifier.Defaults |> Container.From
            legend

        registrationOption.Range <- false

        registrationOption

    interface ISemanticTokensFullHandler with
        member this.GetRegistrationOptions(capability: SemanticTokensCapability, clientCapabilities: ClientCapabilities): SemanticTokensRegistrationOptions = 
            registrationOption

        member this.Handle(request: SemanticTokensParams, cancellationToken: Threading.CancellationToken): Threading.Tasks.Task<SemanticTokens> = 
            let document = SemanticTokensDocument(registrationOption)
            let builder = document.Create()

            let () =
                request.TextDocument.Uri.ToString()
                |> bufferManager.GetBuffer
                |> Option.bind (fun buffer ->
                    match buffer.compilation with
                    | TypeError (ast,_) -> Some ast
                    | Compiled (ast, _) -> Some ast
                    | _ -> None
                )
                |> Option.map (Tokenizer.tokenizeAst)
                |> Option.map (SemanticToken.fromTokens)
                |> Option.iter (fun semanticTokens ->
                    semanticTokens
                    |> Seq.iter (fun semanticToken ->
                        builder.Push(semanticToken.range, semanticToken.tokenType, semanticToken.tokenModifiers)
                    )
                )

            let document = builder.Commit()

            document.GetSemanticTokens()
            |> Threading.Tasks.Task.FromResult
