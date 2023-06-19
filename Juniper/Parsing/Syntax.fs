module Parsing.Syntax

open Parsing.Parser
open SyntaxTree
open Tokens
open Lexer
    
let rec parseModuleDefinition =
    lexerMode ExpressionMode (par {
        let! moduleName = parseModuleName
        let! declarations =
            manyWhile
                (currentKind |> map ((<>) EndOfFileToken))
                parseDeclaration
        return {
            moduleName = moduleName
            declarations = declarations
        }
    })

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
            let! algebraicType = lexerMode TypeMode parseAlgebraicType
            return AlgebraicTypeSyntax algebraicType
        | KeywordToken AliasKeyword ->
            let! alias = lexerMode TypeMode parseAlias
            return AliasSyntax alias
        | KeywordToken FunKeyword ->
            let! func = parseFunctionDeclaration
            return FunctionDeclarationSyntax func
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
    let parseTypeVariableIdentifier =
        par {
            match! currentKind with
            | IdentifierToken ->
                let! typeVariableIdentifier = matchToken TypeVariableIdentifierToken
                let! _skip = nextToken
                return typeVariableIdentifier
            | _ ->
                return! matchToken TypeVariableIdentifierToken
        }
    lexerMode TypeMode (
        pipe3
            (matchToken (LessThanToken))
            (many1Sep parseTypeVariableIdentifier CommaToken)
            (matchToken (GreaterThanToken))
            (fun lt typeVariables gt -> {
                lessThanSign = lt
                typeVariables = typeVariables
                greaterThanSign = gt
            })
    )

and parseTemplateApplication : Parser<TemplateApplicationSyntax> =
    lexerMode TypeMode (
        pipe3
            (matchToken LessThanToken)
            (many1Sep parseTypeExpression CommaToken)
            (matchToken GreaterThanToken)
            (fun lt types gt -> {
                lessThanSign = lt
                templateApplicationTypes = types
                greaterThanSign = gt
            }) 
    )

and parseAlgebraicType : Parser<AlgebraicTypeSyntax> =
    par {
        let! typeKeyword = matchToken (KeywordToken TypeKeyword)
        let! typeIdentifier = matchToken (IdentifierToken)
        let! optionalTemplateDeclaration =
            parseTemplateDec
            |> ifCurrentKind LessThanToken
        let! equals = matchToken EqualsToken
        let skipUnnecessaryFirstPipe =
            par {
                match! currentKind with
                | PipeToken ->
                    // if there's an unnecessary pipe token before the first value constructor, skip it
                    let! _error = matchToken IdentifierToken
                    let! _skip = nextToken
                    return ()
                | _ ->
                    return ()
            }
        do! skipUnnecessaryFirstPipe
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
        let! optionalType = parseOptionalType
        let! equals = matchToken EqualsToken
        let! body = parseExpression
        return {
            letKeyword = letKeyword
            identifier = identifier
            optionalType = optionalType
            equals = equals
            body = body
        }
    }

and parseFunctionDeclaration : Parser<FunctionDeclarationSyntax> =
    par {
        let! funKeyword = matchToken (KeywordToken FunKeyword)
        let! identifier = matchToken IdentifierToken
        let! optionalTemplateDeclaration =
            parseTemplateDec
            |> ifCurrentKind LessThanToken
        let! openParenthesis = matchToken OpenParenthesisToken
        let! functionArguments =
            manySep parseIdentifierWithOptionalType CommaToken CloseParenthesisToken
        let! closeParenthesis = matchToken CloseParenthesisToken
        // TODO constraints
        let! optionalType = parseOptionalType
        let! equal = matchToken EqualsToken
        let! functionBody = parseExpression
        return {
            funKeyword = funKeyword
            identifier = identifier
            optionalTemplateDeclaration = optionalTemplateDeclaration
            openParenthesis = openParenthesis
            functionArguments = functionArguments
            closeParenthesis = closeParenthesis
            optionalType = optionalType
            equals = equal
            functionBody = functionBody
        }
    }

and parseIdentifierWithOptionalType : Parser<IdentifierWithOptionalType> =
    par {
        let! identifier = matchToken IdentifierToken
        let! optionalType = parseOptionalType
        return {
            identifier = identifier
            optionalType = optionalType
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
    lexerMode TypeMode (par {
        match! currentKind with
        | IdentifierToken ->
            return! parseDeclarationReference |> map DeclarationReferenceTypeExpression
        | KeywordToken keyword ->
            match keyword |> SyntaxFacts.getKeywordBaseType with
            | Some baseTy ->
                return! nextToken |> map BuiltInTypeExpression
            | None ->
                let! token = matchToken (KeywordToken UnitKeyword)
                let! _skip = nextToken
                return token |> BuiltInTypeExpression
        | TypeVariableIdentifierToken ->
            return! nextToken |> map TypeVariableIdentifierTypeExpression
        | _ ->
            let! unitType = matchToken (KeywordToken UnitKeyword)
            let! _skip = nextToken
            return unitType |> BuiltInTypeExpression
    })
    

and parseExpression : Parser<ExpressionSyntax> =
    par {
        match! currentKind with
        // | statements?
        | _ -> return! parseBinaryExpression 0
    }

and parseBinaryExpression (parentPrecedence: int) =
    let rec binaryRight left =
        par {
            match! currentKind |> map SyntaxFacts.getBinaryOperatorPrecedence with
            | currentPrecedence when currentPrecedence > parentPrecedence ->
                let! operatorToken = nextToken
                let! right = parseBinaryExpression currentPrecedence
                return! binaryRight (BinaryExpressionSyntax (left, operatorToken, right))
            | _ -> return left
        }

    par {
        let! unaryOperatorPrecedence = currentKind |> map SyntaxFacts.getUnaryOperatorPrecedence
        let! left =
            if unaryOperatorPrecedence <> 0 && unaryOperatorPrecedence > parentPrecedence then
                pipe2
                    nextToken
                    (parseBinaryExpression unaryOperatorPrecedence)
                    (fun operator operand -> UnaryExpressionSyntax (operator, operand))
            else
                parsePrimaryExpression
        return! binaryRight left
    }

(*
while (true)
{
    var precedence = Current.Kind.GetBinaryOperatorPrecedence();
    if (precedence == 0 || precedence <= parentPrecedence)
        break;

    var operatorToken = NextToken();
    var right = ParseBinaryExpression(precedence);
    left = new BinaryExpressionSyntax(_syntaxTree, left, operatorToken, right);
}

return left;
*)

and parsePrimaryExpression : Parser<ExpressionSyntax> =
    par {
        match! currentKind with
        | IdentifierToken ->
            let! declarationReference = parseDeclarationReference
            let! optionalTemplateApplication =
                parseTemplateApplication
                |> ifCurrentKind LessThanToken
            return DeclarationReferenceExpressionSyntax (declarationReference, optionalTemplateApplication)
        | IntLiteralToken ->
            return! nextToken |> map NumberExpressionSyntax
        | StringLiteralToken ->
            return! nextToken |> map StringLiteralExpressionSyntax
        | CharacterArrayLiteralToken ->
            return! nextToken |> map CharacterArrayLiteralExpressionSyntax
        | OpenParenthesisToken ->
            let! openParenthesis = nextToken
            match! currentKind with
            | CloseParenthesisToken ->
                let! closeParenthesis = nextToken
                return UnitLiteralExpression (openParenthesis, closeParenthesis)
            | _ ->
                let! innerExpression = parseExpression
                match! currentKind with
                | SemicolonToken ->
                    let! firstSemicolon = nextToken
                    let! rest = many1Sep parseExpression SemicolonToken
                    let sequence =
                        rest
                        |> SeparatedNonEmprySyntaxList.prepend (innerExpression, firstSemicolon)
                    let! closeParenthesis = matchToken CloseParenthesisToken
                    return SequenceExpression(openParenthesis, sequence, closeParenthesis)
                | CloseParenthesisToken
                | _ ->
                    let! closeParenthesis = matchToken CloseParenthesisToken
                    return ParenthesizedExpressionSyntax (openParenthesis, innerExpression, closeParenthesis)
        | _ -> return! matchToken IntLiteralToken |> map NumberExpressionSyntax
    }

and parseOptionalType : Parser<(Token * TypeExpressionSyntax) option> =
    lexerMode TypeMode (
        pipe2
            (matchToken ColonToken)
            parseTypeExpression
            (fun colon typeExpression -> colon, typeExpression)
        |> ifCurrentKind ColonToken
    )

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