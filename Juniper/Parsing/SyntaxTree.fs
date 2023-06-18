module Parsing.SyntaxTree

open Tokens



type SeparatedSyntaxList<'TItem> =
    | EmptySyntaxList
    | SeparatedSyntaxList of 'TItem * (Token * 'TItem) list

and SeparatedNonEmptySyntaxList<'TItem> = SeparatedNonEmptySyntaxList of 'TItem * (Token * 'TItem) list

type ModuleDefinitionSyntax =
    {
        moduleName: ModuleNameSyntax
        declarations: DeclarationSyntax list
    }
and ModuleNameSyntax =
    {
        moduleKeyword: Token
        moduleNameIdentifier: Token
    }
and DeclarationSyntax =
    | OpenModulesDeclarationSyntax of OpenModulesSyntax
    | AlgebraicTypeSyntax of AlgebraicTypeSyntax
    | FunctionDeclarationSyntax of FunctionDeclarationSyntax
    | LetDeclarationSyntax of LetDeclarationSyntax
    | AliasSyntax of AliasSyntax
and OpenModulesSyntax =
    {
        openKeyword: Token
        openParenthesis: Token
        openedModuleNameIdentifiers: SeparatedSyntaxList<Token>
        closeParenthesis: Token
    } with
    static member Create openKeyword openParenthesis moduleNames closeParenthesis =
        {
            openKeyword = openKeyword
            openParenthesis = openParenthesis
            openedModuleNameIdentifiers = moduleNames
            closeParenthesis = closeParenthesis
        }
and TemplateDeclarationSyntax =
    {
        lessThanSign: Token
        // TODO
        greaterThanSign: Token
    }
and AlgebraicTypeSyntax =
    {
        typeKeyword: Token
        algebraicTypeIdentifier: Token
        optionalTemplateDeclaration: TemplateDeclarationSyntax option
        equals: Token
        valueConstructors: SeparatedNonEmptySyntaxList<ValueConstructorSyntax>
    }
and AliasSyntax =
    {
        aliasKeyword: Token
        aliasIdentifier: Token
        optionalTemplateDeclaration: TemplateDeclarationSyntax option
        equals: Token
        aliasOfType: TypeExpressionSyntax
    }
and ValueConstructorSyntax =
    {
        valueConstructorIdentifier: Token
        openParenthesis: Token
        valueConstructorParameterTypes: SeparatedSyntaxList<TypeExpressionSyntax>
        closeParenthesis: Token
    }
and LetDeclarationSyntax =
    {
        letKeyword: Token
        identifier: Token
        optionalType: (Token * TypeExpressionSyntax) option
        equals: Token
        body: ExpressionSyntax
    }
and FunctionDeclarationSyntax =
    {
        funKeyword: Token
        identifier: Token
        optionalTemplateDeclaration: TemplateDeclarationSyntax option
        openParenthesis: Token
        functionArguments: SeparatedSyntaxList<IdentifierWithOptionalType>
        closeParenthesis: Token
        optionalType: (Token * TypeExpressionSyntax) option
        // TODO function constraint
        equals: Token
        functionBody: ExpressionSyntax
    }
and IdentifierWithOptionalType =
    {
        identifier: Token
        optionalType: (Token * TypeExpressionSyntax) option
    }
and ModuleQualifierSyntax =
    {
        moduleNameIdentifier: Token
        colon: Token
        moduleDeclarationNameIdentifier: Token
    }
and IdentifierDeclarationReferenceSyntax =
    {
        declarationNameIdentifier: Token   
    }

and DeclarationReferenceSyntax =
    | IdentifierDeclarationReference of IdentifierDeclarationReferenceSyntax
    | ModuleQualifierDeclarationReference of ModuleQualifierSyntax

and TypeExpressionSyntax =
    | DeclarationReferenceTypeExpression of DeclarationReferenceSyntax

and ExpressionSyntax =
    | UnitExpression of Token * Token
    | UnaryExpressionSyntax of Token * ExpressionSyntax
    | BinaryExpressionSyntax of ExpressionSyntax * Token * ExpressionSyntax
    | NumberExpressionSyntax of Token
    | ParenthesizedExpressionSyntax of Token * ExpressionSyntax * Token
    
    
type ParseError = PE of (FParsec.Position * FParsec.Position * string * (ParseError list))

type SyntaxTree =
    {
        lexErrors : (FParsec.Position * string) list
        parseErrors: ParseError list
        expression: ModuleDefinitionSyntax
        eofToken: Token
    }
    (*
open Ast

let rec matchesAst (Module ast: Ast.Module) (st: ModuleDefinitionSyntax) =
    let moduleNameMatches =
        ast
        |> Seq.choose (function
            | ModuleNameDec (_, name) -> Some name
            | _ -> None
        )
        |> Seq.tryHead
        |> ((=) st.moduleName.moduleNameIdentifier.text)
    let declarationsMatch () =
        let x = failwith ""
        let decls =
            ast
            |> List.where (function | ModuleNameDec _ -> false | _ -> true)
        let declsMatch =
            decls
            |> Seq.zip st.declarations
            |> Seq.map matchesDeclaration
and matchesDeclaration (declSyntax: Declaration, (_, decl: Declaration)) =
*)