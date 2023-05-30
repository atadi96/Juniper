module JuniperCompiler

open FParsec
open TypedAst

let syntaxCheck (uri: string, text: string): Result<_, (_ * Position option * _)> =
    match runParserOnString Parse.program () uri text with
    | Success (result, _, _) ->
        Result.Ok (Ast.Module result)
    | Failure (errorMsg, parseError, _) ->
        Result.Error (parseError.Position, None, errorMsg)

type TypeCheckedProgram =
    {
        moduleNames: string list
        opens: ModuleDeclaration list
        includes: Declaration list
        typeDecs: ModuleDeclaration list
        inlineCodeDecs: ModuleDeclaration list
        valueSccs: ValueScc list
    }
and ModuleDeclaration =
    {
        moduleName: string
        declaration: Declaration
    }
and ValueScc =
    {
        declarations: ModuleDeclaration list
        theta: Map<string, TyExpr>
        kappa: Map<string, CapacityExpr>
    }

type TypeCheckError =
    | ErrorMessage of Error.ErrorMessage
    | ErrorText of string

type Exception = System.Exception

let typeCheck (uri: string, astModule: Ast.Module): Result<TypeCheckedProgram, TypeCheckError> =
    try
        let moduleDeclarations decs =
            decs
            |> List.map (fun (moduleName, decl) ->
                {
                    moduleName = moduleName
                    declaration = decl
                })
        let (moduleNames, opens, includes, typeDecs, inlineCodeDecs, valueSccs) = TypeChecker.typecheckProgram [astModule] [uri]
        {
            moduleNames =
                moduleNames
            opens = opens |> moduleDeclarations
            includes = includes
            typeDecs = typeDecs |> moduleDeclarations
            inlineCodeDecs = inlineCodeDecs |> moduleDeclarations
            valueSccs =
                valueSccs
                |> List.map (fun (decl,theta,kappa) ->
                    {
                        declarations = decl |> moduleDeclarations
                        theta = theta
                        kappa = kappa
                    }
                )
        }
        |> Result.Ok
    with
    | Error.SemanticError str
    | Program.SyntaxError str
    | Constraint.TypeError str ->
        Result.Error (ErrorText str)
    | Error.SemanticError' errMsg | Error.TypeError' errMsg ->
        Result.Error (ErrorMessage errMsg)
    | e ->
        Result.Error (ErrorText (e.ToString()))