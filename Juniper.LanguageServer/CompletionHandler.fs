namespace Juniper.LanguageServer

open System
open Buffers
open Dependencies
open OmniSharp.Extensions.LanguageServer.Protocol.Document
open OmniSharp.Extensions.LanguageServer.Protocol.Client.Capabilities
open OmniSharp.Extensions.LanguageServer.Protocol.Models

type CompletionHandler(bufferManager: BufferManager, standardLibraryModules: IStandardLibraryModules) =

    let range (pos1: FParsec.Position, pos2: FParsec.Position) =
        let fix (n: int64) = (int n) + 1
        Range(fix pos1.Line, fix pos1.Column, fix pos2.Line, fix pos2.Column)

    let completionItem kind label details =
        let item = CompletionItem()
        item.Kind <- kind
        item.Label <- label
        match details with
        | Some detailText ->
            let markupContent = MarkupContent()
            markupContent.Kind <- MarkupKind.Markdown
            markupContent.Value <- sprintf "%s" detailText
            item.Documentation <- StringOrMarkupContent(markupContent)
        | _ -> ()
        item

    let templateTextAst (template: Ast.Template) =
        let templateParameters =
            template.tyVars
            |> Ast.unwrap
            |> Seq.map (Ast.unwrap)
            |> String.concat ","
        let capactiy =
            template.capVars
            |> Option.map (fun capVars ->
                capVars
                |> Ast.unwrap
                |> Seq.map Ast.unwrap
                |> String.concat ","
            )
            |> Option.defaultValue ""
        sprintf "<%s;%s>" templateParameters capactiy
        
    let templateTextTyped (template: TypedAst.Template) =
        let templateParameters =
            template.tyVars
            |> String.concat ","
        let capactiy =
            template.capVars
            |> String.concat ","
        sprintf "<%s;%s>" templateParameters capactiy

    let getReferencedAstCompletionItems (Ast.Module moduleDeclarations) =
        let declarationsInModule =
            moduleDeclarations
            |> Seq.map snd
            |> Seq.collect (fun declaration ->
                match declaration with
                | Ast.FunctionDec { name = (_, functionName) } -> [ (functionName, CompletionItemKind.Function, None) ]
                | Ast.UnionDec { name = (_, unionName); valCons = (_, valCons); template = template } ->
                    let unionNameText =
                        let templateText =
                            template
                            |> Option.map (Ast.unwrap >> templateTextAst)
                            |> Option.defaultValue ""
                        unionName + templateText
                    valCons
                    |> List.map (fun (name,_ty) ->
                        let name = name |> Ast.unwrap
                        (name, CompletionItemKind.EnumMember, Some (sprintf "```union case %s : '? -> %s```" name unionNameText))
                    )
                    |> fun cons -> (unionName, CompletionItemKind.Class, None) :: cons
                | Ast.AliasDec { name = (_, aliasName) } -> [ (aliasName, CompletionItemKind.Class, None) ]
                | Ast.LetDec { varName = (_, varName) } -> [ (varName, CompletionItemKind.Variable, None) ]
                | Ast.ModuleNameDec _ -> []
                | Ast.OpenDec _ -> []
                | Ast.IncludeDec _ -> []
                | Ast.InlineCodeDec _ -> []
            )
            |> Seq.map (fun (name, kind, description) ->
                completionItem kind name description
            )
        declarationsInModule
        |> CompletionList

    let getTypedDeclarationCompletionItems (declarations: TypedAst.Declaration seq) =
        declarations
        |> Seq.collect (function
            | TypedAst.FunctionDec functionDec ->
                let templateText =
                    functionDec.template
                    |> Option.map (templateTextTyped)
                    |> Option.defaultValue ""
                let documentationString =
                    seq {
                        yield "```fs"
                        yield sprintf "val %s %s :" functionDec.name templateText
                        yield 
                            functionDec.clause.arguments
                            |> Seq.map (fun (argName, argTy) ->
                                sprintf "  %s : %s" argName (TypedAst.typeString argTy)
                            )
                            |> String.concat (sprintf " ->%s" System.Environment.NewLine)
                        yield sprintf "  -> %s" (TypedAst.typeString functionDec.clause.returnTy)
                        yield  "```"
                    }
                    |> String.concat System.Environment.NewLine
                [ (functionDec.name, CompletionItemKind.Function, Some documentationString) ]
            | TypedAst.UnionDec unionDec ->
                let unionNameText =
                    let templateText =
                        unionDec.template
                        |> Option.map (templateTextTyped)
                        |> Option.defaultValue ""
                    unionDec.name + templateText
                let unionCaseDecs =
                    unionDec.valCons
                    |> List.map (fun (name,types) ->
                        let typeText =
                            match types with
                            | [] -> "unit"
                            | _ ->
                                types
                                |> Seq.map (TypedAst.typeString)
                                |> String.concat " * "
                        (name, CompletionItemKind.EnumMember, Some (sprintf "`union case %s : %s -> %s`" name typeText unionNameText))
                    )
                let unionDocumentation =
                    seq {
                        yield "```fs"
                        yield sprintf "union %s =" unionNameText
                        for (unionCase, types) in unionDec.valCons do
                            let typeText =
                                types
                                |> Seq.map (TypedAst.typeString)
                                |> String.concat " * "
                            yield sprintf "  | %s (%s)" unionCase typeText
                        yield "```"
                    }
                    |> String.concat System.Environment.NewLine
                let union = (unionDec.name, CompletionItemKind.Class, Some unionDocumentation)
                union :: unionCaseDecs
            | TypedAst.AliasDec aliasDec -> [ (aliasDec.name, CompletionItemKind.Class, None) ]
            | TypedAst.LetDec letDec -> [ (letDec.varName, CompletionItemKind.Variable, None) ]
            | _ -> []
        )
        |> Seq.map (fun (name, kind, description) ->
            completionItem kind name description
        )
        |> CompletionList

    interface ICompletionHandler with
        member this.GetRegistrationOptions(capability: CompletionCapability, clientCapabilities: ClientCapabilities): CompletionRegistrationOptions = 
            let options = CompletionRegistrationOptions()
            options.DocumentSelector <- junDocumentSelector
            options.ResolveProvider <- false
            options.TriggerCharacters <- Container.From [":"]
            options

        member this.Handle(request: CompletionParams, cancellationToken: Threading.CancellationToken): Threading.Tasks.Task<CompletionList> = 
            let documentPath = request.TextDocument.Uri.ToString()

            let buffer = bufferManager.GetBuffer(documentPath)

            let defaultItem =
                let item = CompletionItem()
                item.Kind <- CompletionItemKind.Function
                item.Label <- ""
                item

            match buffer with
            | Some { compilation = TypeError (_, Some (Error.DeclarationNotFoundInModule (moduleDeclPos, moduleName))) } -> // when (range moduleDeclPos).Contains request.Position ->
                match bufferManager.TryGetModule moduleName with
                | Some { lastAst = Some (moduleDef, _text) } ->
                    moduleDef
                    |> getReferencedAstCompletionItems
                | Some _ -> CompletionList()
                | None ->
                    let typeCheckedStdLib = standardLibraryModules.GetTypeCheckedStandardLibrary()
                    typeCheckedStdLib
                    |> JuniperCompiler.TypeCheckedProgram.getModuleDeclarations moduleName
                    |> getTypedDeclarationCompletionItems

                |> Threading.Tasks.Task.FromResult

            | _ -> CompletionList(defaultItem) |> Threading.Tasks.Task.FromResult
