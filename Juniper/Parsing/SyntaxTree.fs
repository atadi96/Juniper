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
    | LetDeclarationSyntax of LetDeclarationSyntax
    | AlgebraicTypeSyntax of AlgebraicTypeSyntax
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

