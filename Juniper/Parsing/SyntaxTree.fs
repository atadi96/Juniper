module Parsing.SyntaxTree

open Tokens



type SeparatedSyntaxList<'TItem> =
    | EmptySyntaxList
    | SeparatedSyntaxList of 'TItem * (Token * 'TItem) list

and SeparatedNonEmptySyntaxList<'TItem> = SeparatedNonEmptySyntaxList of 'TItem * (Token * 'TItem) list

module SeparatedNonEmprySyntaxList =
    let prepend (item, sep) (SeparatedNonEmptySyntaxList (firstItem, followingItems)) =
        SeparatedNonEmptySyntaxList (item, (sep, firstItem) :: followingItems)
    let singleton item = SeparatedNonEmptySyntaxList (item, [])
    let toSeparatedSyntaxList (SeparatedNonEmptySyntaxList (firstItem, followingItems)) =
        SeparatedSyntaxList (firstItem, followingItems)

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
    | InlineCppDeclarationSyntax of Token
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
        typeVariables: SeparatedNonEmptySyntaxList<Token>
        greaterThanSign: Token
    }
and TemplateApplicationSyntax =
    {
        lessThanSign: Token
        templateApplicationTypes: SeparatedNonEmptySyntaxList<TypeExpressionSyntax>
        // TODO capacities
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

and FunctionTypeExpressionSyntax =
    {
        closureOpenParenthesis: Token
        closureOfFunction: ClosureOfFunctionSyntax
        closureCloseParenthesis: Token
        argumentTypesOpenParenthesis: Token
        argumentTypes: SeparatedSyntaxList<TypeExpressionSyntax>
        argumentTypesCloseParenthesis: Token
        arrow: Token
        returnType: TypeExpressionSyntax
    }
and ClosureTypeExpressionSyntax =
    {
        openPipe: Token
        capturedVariables: SeparatedSyntaxList<IdentifierWithType>
        closePipe: Token
    }
and IdentifierWithType =
    {
        identifier: Token
        colon: Token
        requiredType: TypeExpressionSyntax
    }
and ClosureOfFunctionSyntax =
    | ClosureTypeExpression of ClosureTypeExpressionSyntax
    | ClosureTypeVariable of Token

and RecordTypeExpressionSyntax =
    {
        packed: Token option
        openBrace: Token
        recordMemberTypes: SeparatedSyntaxList<IdentifierWithType>
        closeBrace: Token
    }

and TypeExpressionSyntax =
    | DeclarationReferenceTypeExpression of DeclarationReferenceSyntax
    | BuiltInTypeExpression of Token
    | TypeVariableIdentifierTypeExpression of Token
    | FunctionTypeExpression of FunctionTypeExpressionSyntax
    | ParenthesizedTypeExpressionSyntax of Token * TypeExpressionSyntax * Token
    | RecordTypeExpression of RecordTypeExpressionSyntax

and FunctionCallExpressionSyntax =
    {
        functionExpression: ExpressionSyntax
        openParenthesis: Token
        functionCallArguments: SeparatedSyntaxList<ExpressionSyntax>
        closeParenthesis: Token
    }

and LetExpressionSyntax =
    {
        letKeyword: Token
        pattern: PatternSyntax
        equals: Token
        body: ExpressionSyntax
    }

and LambdaExpressionSyntax =
    {
        fnKeyword: Token
        openParenthesis: Token
        lambdaArguments: SeparatedSyntaxList<IdentifierWithOptionalType>
        closeParenthesis: Token
        optionalReturnType: (Token * TypeExpressionSyntax) option
        arrow: Token
        lambdaBodyExpression: ExpressionSyntax
        endKeyword: Token
    }

and CaseOfExpressionSyntax =
    {
        caseKeyword: Token
        matchExpression: ExpressionSyntax
        ofKeyword: Token
        firstPipe: Token
        caseClauses: SeparatedNonEmptySyntaxList<CaseClauseSyntax>
        endKeyword: Token
    }

and ExpressionSyntax =
    | UnitLiteralExpression of Token * Token
    | BoolLiteralExpression of Token
    | NullLiteralExpression of Token
    | UnaryExpressionSyntax of Token * ExpressionSyntax
    | BinaryExpressionSyntax of ExpressionSyntax * Token * ExpressionSyntax
    | NumberExpressionSyntax of Token
    | ParenthesizedExpressionSyntax of Token * ExpressionSyntax * Token
    | SequenceExpression of Token * SeparatedNonEmptySyntaxList<ExpressionSyntax> * Token
    | StringLiteralExpressionSyntax of Token
    | CharacterArrayLiteralExpressionSyntax of Token
    | DeclarationReferenceExpressionSyntax of DeclarationReferenceSyntax * (TemplateApplicationSyntax option)
    | CaseOfExpression of CaseOfExpressionSyntax
    | InlineCppExpressionSyntax of Token
    | FunctionCallExpression of FunctionCallExpressionSyntax
    | LetExpression of LetExpressionSyntax
    | LambdaExpression of LambdaExpressionSyntax
    
and CaseClauseSyntax =
    {
        pattern: PatternSyntax
        doubleArrow: Token
        expression: ExpressionSyntax
    }
    
and VariablePatternSyntax =
    {
        mutableKeyword: Token option
        variablePatternIdentifier: Token
        optionalType: (Token * TypeExpressionSyntax) option
    }

and ValConPatternSyntax =
    {
        valConDeclarationReference: DeclarationReferenceSyntax
        optionalTemplateApplication: TemplateApplicationSyntax option
        openParenthesis: Token
        valConArguments: SeparatedSyntaxList<PatternSyntax>
        closeParenthesis: Token
    }

and PatternSyntax =
    | VariablePattern of VariablePatternSyntax
    | IntegerPattern of Token
    | TruePattern of Token
    | FalsePattern of Token
    | UnderscorePattern of Token
    | UnitPattern of Token * Token
    | ValConPattern of ValConPatternSyntax

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