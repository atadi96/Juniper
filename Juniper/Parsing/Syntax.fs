module Parsing.Syntax

open Parsing.Parser
open SyntaxTree
open Tokens
open Lexer


let rec primaryExpression : Parser<ExpressionSyntax> =
    par {
        let! currentToken = current
        if currentToken.tokenKind = TokenKind.OpenParenthesisToken then
            let! openPar = nextToken
            let! expr = expression
            let! closePar = matchToken TokenKind.CloseParenthesisToken
            return ParenthesizedExpressionSyntax (openPar, expr, closePar)
        else
            let! numberToken = matchToken TokenKind.IntLiteralToken
            return NumberExpressionSyntax numberToken
    }

and expression : Parser<ExpressionSyntax> = term

and factor : Parser<ExpressionSyntax> =
    let rec factorRight left =
        par {
            match! currentKind with
            | SlashToken
            | StarToken ->
                let! operatorToken = nextToken
                let! right = primaryExpression
                return! factorRight (BinaryExpressionSyntax (left, operatorToken, right))
            | _ -> return left
        }
    primaryExpression |> bind factorRight

and term : Parser<ExpressionSyntax> =
    let rec termRight left =
        par {
            match! currentKind with
            | PlusToken
            | MinusToken ->
                let! operatorToken = nextToken
                let! right = factor
                return! termRight (BinaryExpressionSyntax (left, operatorToken, right))
            | _ -> return left
        }
    factor |> bind termRight
    
and parseModuleDefinition =
    par {
        let! moduleName = parseModuleName
        let! declarations =
            manyWhile
                (currentKind |> map ((<>) EndOfFileToken))
                parseDeclaration
        return {
            moduleName = moduleName
            declarations = declarations
        }
    }

and parseModuleName : Parser<ModuleNameSyntax> =
    par {
        let! moduleKeyword = matchToken (KeywordToken ModuleKeyword)
        let! moduleName = matchToken (IdentifierToken)
        return {
            moduleKeyword = moduleKeyword
            moduleNameIdentifier = moduleName
        }
    }

and parseDeclaration : Parser<DeclarationSyntax> =
    par {
        match! currentKind with
        | KeywordToken OpenKeyword ->
            let! openModules = parseOpenModules
            return OpenModulesDeclarationSyntax openModules
        | KeywordToken LetKeyword ->
            let! letDeclaration = parseLetDeclaration
            return LetDeclarationSyntax letDeclaration
        | KeywordToken TypeKeyword ->
            let! algebraicType = parseAlgebraicType
            return AlgebraicTypeSyntax algebraicType
        | KeywordToken AliasKeyword ->
            let! alias = parseAlias
            return AliasSyntax alias
        | _ ->
            // token is not recognized as top level declaration: skip it
            // give a syntax error
            let! _ = matchToken (KeywordToken LetKeyword)
            // and consume the unexpected token
            let! _ = nextToken
            // and try again if it's not the end of the file
            match! currentKind with
            | EndOfFileToken ->
                // if it's EOF, let's report that we're wanting a let declaration so that we can return something
                let! letDeclaration = parseLetDeclaration
                return LetDeclarationSyntax letDeclaration
            | _ ->
                //otherwise just try again
                return! parseDeclaration
    }

and parseOpenModules : Parser<OpenModulesSyntax> =
    pipe4
        (matchToken (KeywordToken OpenKeyword))
        (matchToken OpenParenthesisToken)
        (manySep (matchToken IdentifierToken) CommaToken CloseParenthesisToken)
        (matchToken CloseParenthesisToken)
        OpenModulesSyntax.Create

and parseTemplateDec : Parser<TemplateDeclarationSyntax> =
    pipe2
        (matchToken (LessThanToken))
        (matchToken (GreaterThanToken))
        (fun lt gt -> {
            lessThanSign = lt
            greaterThanSign = gt
        })

and parseAlgebraicType : Parser<AlgebraicTypeSyntax> =
    par {
        let! typeKeyword = matchToken (KeywordToken TypeKeyword)
        let! typeIdentifier = matchToken (IdentifierToken)
        let! optionalTemplateDeclaration =
            parseTemplateDec
            |> ifCurrentKind LessThanToken
        let! equals = matchToken EqualsToken
        let! valueConstructors =
            many1Sep parseValueConstructor PipeToken
        return {
            typeKeyword = typeKeyword
            algebraicTypeIdentifier = typeIdentifier
            optionalTemplateDeclaration = optionalTemplateDeclaration
            equals = equals
            valueConstructors = valueConstructors
        }
    }

and parseAlias : Parser<AliasSyntax> =
    par {
        let! aliasKeyword = matchToken (KeywordToken AliasKeyword)
        let! aliasIdentifier = matchToken (IdentifierToken)
        let! optionalTemplateDeclaration =
            parseTemplateDec
            |> ifCurrentKind LessThanToken
        let! equals = matchToken EqualsToken
        let! aliasOfType = parseTypeExpression
        return {
            aliasKeyword = aliasKeyword
            aliasIdentifier = aliasIdentifier
            optionalTemplateDeclaration = optionalTemplateDeclaration
            equals = equals
            aliasOfType = aliasOfType
        }
    }

and parseValueConstructor : Parser<ValueConstructorSyntax> =
    par {
        let! valueConstructorIdentifier = matchToken IdentifierToken
        let! openParenthesis = matchToken OpenParenthesisToken
        let! valueConstructorParameterTypes =
            manySep parseTypeExpression CommaToken CloseParenthesisToken
        let! closeParenthesis = matchToken CloseParenthesisToken
        return {
            valueConstructorIdentifier = valueConstructorIdentifier
            openParenthesis = openParenthesis
            valueConstructorParameterTypes = valueConstructorParameterTypes
            closeParenthesis = closeParenthesis
        }
    }

and parseLetDeclaration : Parser<LetDeclarationSyntax> =
    par {
        let! letKeyword = matchToken (KeywordToken LetKeyword)
        let! identifier = matchToken IdentifierToken
        let! optionalType =
            pipe2
                (matchToken ColonToken)
                (parseTypeExpression)
                (fun colon typeExpression -> colon, typeExpression)
            |> ifCurrentKind ColonToken
        let! equals = matchToken EqualsToken
        let! body = expression
        return {
            letKeyword = letKeyword
            identifier = identifier
            optionalType = optionalType
            equals = equals
            body = body
        }
    }

and parseDeclarationReference : Parser<DeclarationReferenceSyntax> =
    par {
        let! identifier = matchToken IdentifierToken
        let optionalModuleDeclarationAccess =
            pipe2
                (matchToken ColonToken)
                (matchToken IdentifierToken)
                (fun colon moduleDeclarationName -> colon, moduleDeclarationName)
            |> ifCurrentKind ColonToken
        match! optionalModuleDeclarationAccess with
        | Some (colon, moduleDeclarationNameIdentifier) ->
            return ModuleQualifierDeclarationReference {
                moduleNameIdentifier = identifier
                colon = colon
                moduleDeclarationNameIdentifier = moduleDeclarationNameIdentifier
            }
        | _ ->
            return IdentifierDeclarationReference {
                declarationNameIdentifier = identifier
            }
    }

and parseTypeExpression : Parser<TypeExpressionSyntax> =
    parseDeclarationReference |> map DeclarationReferenceTypeExpression

and syntaxTree : Parser<SyntaxTree> =
    par {
        let! module_ = parseModuleDefinition
        let! eofToken = matchToken TokenKind.EndOfFileToken
        let! (parseErrors, lexErrors) = errors
        return { lexErrors = lexErrors; parseErrors = parseErrors; expression = module_; eofToken = eofToken }
    }

let parse streamName text =
    let lexer = Lexer(streamName, text)
    let noToken = Unchecked.defaultof<Token>
    let (_noToken, initialState) = nextToken (lexer, ParserState(noToken, []))
    let (result, _) = syntaxTree (lexer, initialState)
    result