module Parsing.SyntaxTree

open Tokens

type SeparatedSyntaxList<'TItem> =
    | EmptySyntaxList
    | SeparatedSyntaxList of 'TItem * (Token * 'TItem) list

and SeparatedNonEmptySyntaxList<'TItem> = SeparatedNonEmptySyntaxList of 'TItem * (Token * 'TItem) list

module SeparatedNonEmptySyntaxList =
    let prepend (item, sep) (SeparatedNonEmptySyntaxList (firstItem, followingItems)) =
        SeparatedNonEmptySyntaxList (item, (sep, firstItem) :: followingItems)
    let singleton item = SeparatedNonEmptySyntaxList (item, [])
    let toSeparatedSyntaxList (SeparatedNonEmptySyntaxList (firstItem, followingItems)) =
        SeparatedSyntaxList (firstItem, followingItems)
    let toSeq fSeparator fItem (SeparatedNonEmptySyntaxList (firstItem, followingItems)) =
        seq {
            yield firstItem |> fItem
            for (token,item) in followingItems do
                yield token |> fSeparator
                yield item |> fItem
        }

module SeparatedSyntaxList =
    let toSeq fSeparator fItem separatedSyntaxList =
        match separatedSyntaxList with
        | EmptySyntaxList -> Seq.empty
        | SeparatedSyntaxList (item,items) ->
            SeparatedNonEmptySyntaxList (item,items)
            |> SeparatedNonEmptySyntaxList.toSeq fSeparator fItem

type ModuleQualifierSyntax =
    {
        moduleNameIdentifier: Token
        colon: Token
        moduleDeclarationNameIdentifier: Token
    }
type IdentifierDeclarationReferenceSyntax =
    {
        declarationNameIdentifier: Token   
    }

type DeclarationReferenceSyntax =
    | IdentifierDeclarationReference of IdentifierDeclarationReferenceSyntax
    | ModuleQualifierDeclarationReference of ModuleQualifierSyntax

module Types =

    type RecordTypeExpressionSyntax =
        {
            packed: Token option
            openBrace: Token
            recordMemberTypes: SeparatedSyntaxList<IdentifierWithType>
            closeBrace: Token
        }

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

    and TypeExpressionSyntax =
        | DeclarationReferenceTypeExpression of DeclarationReferenceSyntax
        | BuiltInTypeExpression of Token
        | TypeVariableIdentifierTypeExpression of Token
        | FunctionTypeExpression of FunctionTypeExpressionSyntax
        | ParenthesizedTypeExpressionSyntax of Token * TypeExpressionSyntax * Token
        | RecordTypeExpression of RecordTypeExpressionSyntax
        

    and IdentifierWithType =
        {
            identifier: Token
            colon: Token
            requiredType: TypeExpressionSyntax
        }

    and ClosureTypeExpressionSyntax =
        {
            openPipe: Token
            capturedVariables: SeparatedSyntaxList<IdentifierWithType>
            closePipe: Token
        }

    and ClosureOfFunctionSyntax =
        | ClosureTypeExpression of ClosureTypeExpressionSyntax
        | ClosureTypeVariable of Token

    type IdentifierWithOptionalType =
        {
            identifier: Token
            optionalType: (Token * TypeExpressionSyntax) option
        }

type TemplateApplicationSyntax =
    {
        lessThanSign: Token
        templateApplicationTypes: SeparatedNonEmptySyntaxList<Types.TypeExpressionSyntax>
        // TODO capacities
        greaterThanSign: Token
    }

module Patterns =
        
    type VariablePatternSyntax =
        {
            mutableKeyword: Token option
            variablePatternIdentifier: Token
            optionalType: (Token * Types.TypeExpressionSyntax) option
        }
    
    type ValConPatternSyntax =
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
        

module Expressions =
    type CaseClauseSyntax =
        {
            pattern: Patterns.PatternSyntax
            doubleArrow: Token
            expression: ExpressionSyntax
        }

    
    and FunctionCallExpressionSyntax =
        {
            functionExpression: ExpressionSyntax
            openParenthesis: Token
            functionCallArguments: SeparatedSyntaxList<ExpressionSyntax>
            closeParenthesis: Token
        }
    
    and IfExpressionSyntax =
        {
            ifKeyword: Token
            conditionExpression: ExpressionSyntax
            thenKeyword: Token
            trueExpression: ExpressionSyntax
            elifBranches: ElifBranchSyntax list
            elseKeyword: Token
            falseExpression: ExpressionSyntax
            endKeyword: Token
        }
        
    and ElifBranchSyntax =
        {
            elifKeyword: Token
            conditionExpression: ExpressionSyntax
            thenKeyword: Token
            trueExpression: ExpressionSyntax
        }
    
    and LetExpressionSyntax =
        {
            letKeyword: Token
            pattern: Patterns.PatternSyntax
            equals: Token
            body: ExpressionSyntax
        }
    
    and VariableDeclarationSyntax =
        {
            varKeyword: Token
            variableIdentifier: Token
            colonToken: Token
            variableType: Types.TypeExpressionSyntax
        }
    
    and SetExpressionSyntax =
        {
            setKeyword: Token
            optionalRefKeyword: Token option
            leftAssign: LeftAssignSyntax
            equalsToken: Token
            expression: ExpressionSyntax
        }
    
    and ForLoopExpressionSyntax =
        {
            forKeyword: Token
            identifier: Token
            optionalTyExpr: (Token * Types.TypeExpressionSyntax) option
            inKeyword: Token
            startExpression: ExpressionSyntax
            direction: Token
            endExpression: ExpressionSyntax
            doKeyword: Token
            bodyExpression: ExpressionSyntax
            endKeyword: Token
        }
    
    and DoWhileExpressionSyntax =
        {
            doKeyword: Token
            bodyExpression: ExpressionSyntax
            whileKeyword: Token
            conditionExpression: ExpressionSyntax
            endKeyword: Token
        }
    
    and WhileDoExpressionSyntax =
        {
            whileKeyword: Token
            conditionExpression: ExpressionSyntax
            doKeyword: Token
            bodyExpression: ExpressionSyntax
            endKeyword: Token
        }
    
    and LambdaExpressionSyntax =
        {
            fnKeyword: Token
            openParenthesis: Token
            lambdaArguments: SeparatedSyntaxList<Types.IdentifierWithOptionalType>
            closeParenthesis: Token
            optionalReturnType: (Token * Types.TypeExpressionSyntax) option
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
        | IfExpression of IfExpressionSyntax
        | LetExpression of LetExpressionSyntax
        | VariableDeclaration of VariableDeclarationSyntax
        | SetExpression of SetExpressionSyntax
        | ForLoopExpression of ForLoopExpressionSyntax
        | DoWhileExpression of DoWhileExpressionSyntax
        | WhileDoExpression of WhileDoExpressionSyntax
        | LambdaExpression of LambdaExpressionSyntax
    
    and ArrayAccessLeftAssignSyntax =
        {
            arrayLeftAssign: LeftAssignSyntax
            openBracket: Token
            indexExpression: ExpressionSyntax
            closeBracket: Token
        }
    
    and RecordMemberLeftAssignSyntax =
        {
            recordLeftAssign: LeftAssignSyntax
            memberAccess: Token
            memberIdentifier: Token
        }
    
    and LeftAssignSyntax =
        | DeclarationReferenceLeftAssign of DeclarationReferenceSyntax
        | ArrayAccessLeftAssign of ArrayAccessLeftAssignSyntax
        | RecordMemberLeftAssign of RecordMemberLeftAssignSyntax

module Declarations =
    type OpenModulesSyntax =
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
    type TemplateDeclarationSyntax =
        {
            lessThanSign: Token
            typeVariables: SeparatedNonEmptySyntaxList<Token>
            greaterThanSign: Token
        }
    type ValueConstructorSyntax =
        {
            valueConstructorIdentifier: Token
            openParenthesis: Token
            valueConstructorParameterTypes: SeparatedSyntaxList<Types.TypeExpressionSyntax>
            closeParenthesis: Token
        }
    type AlgebraicTypeSyntax =
        {
            typeKeyword: Token
            algebraicTypeIdentifier: Token
            optionalTemplateDeclaration: TemplateDeclarationSyntax option
            equals: Token
            valueConstructors: SeparatedNonEmptySyntaxList<ValueConstructorSyntax>
        }
    type AliasSyntax =
        {
            aliasKeyword: Token
            aliasIdentifier: Token
            optionalTemplateDeclaration: TemplateDeclarationSyntax option
            equals: Token
            aliasOfType: Types.TypeExpressionSyntax
        }
    type LetDeclarationSyntax =
        {
            letKeyword: Token
            identifier: Token
            optionalType: (Token * Types.TypeExpressionSyntax) option
            equals: Token
            body: Expressions.ExpressionSyntax
        }
    type FunctionDeclarationSyntax =
        {
            funKeyword: Token
            identifier: Token
            optionalTemplateDeclaration: TemplateDeclarationSyntax option
            openParenthesis: Token
            functionArguments: SeparatedSyntaxList<Types.IdentifierWithOptionalType>
            closeParenthesis: Token
            optionalType: (Token * Types.TypeExpressionSyntax) option
            // TODO function constraint
            equals: Token
            functionBody: Expressions.ExpressionSyntax
        }
    
    type DeclarationSyntax =
        | OpenModulesDeclarationSyntax of OpenModulesSyntax
        | AlgebraicTypeSyntax of AlgebraicTypeSyntax
        | FunctionDeclarationSyntax of FunctionDeclarationSyntax
        | LetDeclarationSyntax of LetDeclarationSyntax
        | AliasSyntax of AliasSyntax
        | InlineCppDeclarationSyntax of Token

module Modules =
    type ModuleDefinitionSyntax =
        {
            moduleName: ModuleNameSyntax
            declarations: Declarations.DeclarationSyntax list
        }
            
    and ModuleNameSyntax =
        {
            moduleKeyword: Token
            moduleNameIdentifier: Token
        }

type ParseError = PE of (FParsec.Position * FParsec.Position * string * (ParseError list))

type SyntaxTree =
    {
        lexErrors : (FParsec.Position * string) list
        parseErrors: ParseError list
        expression: Modules.ModuleDefinitionSyntax
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