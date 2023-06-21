module Parsing.Syntax

open Parsing.Parser
open SyntaxTree
open Tokens
open Lexer
    
type ExpressionLeftRecursion =
    | FunctionCallLeftRecursion of Token * SeparatedSyntaxList<ExpressionSyntax> * Token

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
    let isDeclarationStart tokenKind =
        match tokenKind with
        | KeywordToken OpenKeyword
        | KeywordToken LetKeyword
        | KeywordToken TypeKeyword
        | KeywordToken AliasKeyword
        | KeywordToken FunKeyword
        | InlineCppToken -> true
        | _ -> false

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
        | InlineCppToken ->
            let! inlineCpp = nextToken
            return InlineCppDeclarationSyntax inlineCpp
        | _ ->
            // token is not recognized as top level declaration: skip to the next possible declaration
            // give a syntax error
            let! _ = matchToken (KeywordToken LetKeyword)
            // and skip all tokens until we find a token that can start a declaration, or reach the end of the token
            let tokenKindShouldBeSkipped tokenKind =
                tokenKind <> EndOfFileToken && not (tokenKind |> isDeclarationStart)

            let! _ = manyWhile (currentKind |> map tokenKindShouldBeSkipped) skipSilent

            // TODO another recovery tactic: if we find a = we can try to parse an expression or a type, though which one I don't know

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
                let! () = skipSilent
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
                    do! skipSilent
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

and parseClosureTypeExpression : Parser<ClosureTypeExpressionSyntax> =
    par {
        let! openPipe = matchToken PipeToken
        let! capturedVariables =
            manySep parseIdentifierWithType SemicolonToken PipeToken
        let! closePipe = matchToken PipeToken
        return {
            openPipe = openPipe
            capturedVariables = capturedVariables
            closePipe = closePipe
        }
    }

and parseFunctionTypeExpressionStartingFromArguments : Parser<Token * ClosureOfFunctionSyntax * Token -> FunctionTypeExpressionSyntax> =
    par {
        let! argumentsOpenParenthesis = matchToken OpenParenthesisToken
        let! argumentTypes = manySep parseTypeExpression CommaToken CloseParenthesisToken
        let! argumentsCloseParenthesis = matchToken CloseParenthesisToken
        let! arrowToken = matchToken ArrowToken
        let! returnType = parseTypeExpression
        return fun (openClosure, closure, closeClosure) ->
            {
                closureOpenParenthesis = openClosure
                closureOfFunction = closure
                closureCloseParenthesis = closeClosure
                argumentTypesOpenParenthesis = argumentsOpenParenthesis
                argumentTypes = argumentTypes
                argumentTypesCloseParenthesis = argumentsCloseParenthesis
                arrow = arrowToken
                returnType = returnType
            }
    }

and parseTypeExpression : Parser<TypeExpressionSyntax> =
    lexerMode TypeMode (par {
        match! currentKind with
        | IdentifierToken ->
            return! parseDeclarationReference |> map DeclarationReferenceTypeExpression
        | TypeVariableIdentifierToken ->
            return! nextToken |> map TypeVariableIdentifierTypeExpression
        | OpenParenthesisToken ->
            let! openParenthesis = nextToken
            match! currentKind with
            | PipeToken ->
                // (|
                //  ^
                // we parse a closure type as part of a function type
                let! closureType = parseClosureTypeExpression
                let! closeParenthesis = matchToken CloseParenthesisToken
                let! restOfFunctionType = parseFunctionTypeExpressionStartingFromArguments
                return
                    (openParenthesis, ClosureTypeExpression closureType, closeParenthesis)
                    |> restOfFunctionType
                    |> FunctionTypeExpression
            | _ ->
                // (
                //  ^
                // which is either parenthesized type expression or ('closure)(...) -> <ty-expr> case of function types
                let! innerType = parseTypeExpression
                let! closeParenthesis = matchToken CloseParenthesisToken
                let! currentTokenKind = currentKind
                match! ret (innerType, currentTokenKind) with
                | (TypeVariableIdentifierTypeExpression typeVariable, OpenParenthesisToken) ->
                    // currently we see this
                    // ('typeVar)(
                    //           ^
                    // this is one of the two valid cases of function type, so let's parse that
                    let! restOfFunctionType = parseFunctionTypeExpressionStartingFromArguments
                    return
                        (openParenthesis, ClosureTypeVariable typeVariable, closeParenthesis)
                        |> restOfFunctionType
                        |> FunctionTypeExpression
                | _ ->
                    // (<ty-expr>)
                    //            ^
                    return
                        (openParenthesis, innerType, closeParenthesis)
                        |> ParenthesizedTypeExpressionSyntax
        | OpenBraceToken
        | KeywordToken PackedKeyword ->
            // [packed] "{"
            let! packed =
                matchToken (KeywordToken PackedKeyword)
                |> ifCurrentKind (KeywordToken PackedKeyword)
            let! openBrace = matchToken OpenBraceToken
            let! recordMemberTypes =
                par {
                    match! currentKind with
                    | CloseBraceToken ->
                        return EmptySyntaxList
                    | _ ->
                        return!
                            many1Sep parseIdentifierWithType SemicolonToken
                            |> map SeparatedNonEmprySyntaxList.toSeparatedSyntaxList
                }
            let! closeBrace = matchToken CloseBraceToken
            return RecordTypeExpression {
                packed = packed
                openBrace = openBrace
                recordMemberTypes = recordMemberTypes
                closeBrace = closeBrace
            }
        | KeywordToken keyword ->
            match keyword |> SyntaxFacts.getKeywordBaseType with
            | Some baseTy ->
                return! nextToken |> map BuiltInTypeExpression
            | None ->
                let! token = matchToken (KeywordToken UnitKeyword)
                let! () = skipSilent
                return token |> BuiltInTypeExpression
        | _ ->
            let! unitType = matchToken (KeywordToken UnitKeyword)
            //let! _skip = skipSilent // this hurts recovery from e.g. "alias a = { a: a; a: }" so let's turn it off for now? idk why it was added
            return unitType |> BuiltInTypeExpression
    })
    

and parseExpression : Parser<ExpressionSyntax> =
    par {
        match! currentKind with
        // | statements?
        | _ ->
            return! parseBinaryExpression 0
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
    let tryParseOneLeftRecursiveTerm: Parser<ExpressionLeftRecursion option> =
        par {
            match! currentKind with
            | OpenParenthesisToken ->
                let! openParenthesis = nextToken
                let! arguments = manySep parseExpression CommaToken CloseParenthesisToken
                let! closeParenthesis = matchToken CloseParenthesisToken
                let leftRecursion = FunctionCallLeftRecursion (openParenthesis, arguments, closeParenthesis)
                return Some leftRecursion
            | _ ->
                return None
        }
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
        | InlineCppToken ->
            return! matchToken InlineCppToken |> map InlineCppExpressionSyntax
        | KeywordToken LetKeyword ->
            let! letKeyword = nextToken
            let! pattern = parsePattern
            let! equals = matchToken EqualsToken
            let! body = parseExpression
            return LetExpression {
                letKeyword = letKeyword
                pattern = pattern
                equals = equals
                body = body
            }
        | KeywordToken FnKeyword ->
            let! fnKeyword = nextToken
            let! openParenthesis = matchToken OpenParenthesisToken
            let! arguments = manySep parseIdentifierWithOptionalType CommaToken CloseParenthesisToken
            let! closeParenthesis = matchToken CloseParenthesisToken
            let! optionalReturnType = parseOptionalType
            let! arrow = matchToken ArrowToken
            let! bodyExpression = parseExpression
            let! endKeyword = matchToken (KeywordToken EndKeyword)
            return LambdaExpression {
                fnKeyword = fnKeyword
                openParenthesis = openParenthesis
                lambdaArguments = arguments
                closeParenthesis = closeParenthesis
                optionalReturnType = optionalReturnType
                arrow = arrow
                lambdaBodyExpression = bodyExpression
                endKeyword = endKeyword
            }
        (*
        | OpenBracketToken ->
            let! openBracket = nextToken
            let! listElements = many1Sep parseExpression CommaToken
            let! closeBracket = matchToken CloseBracketToken
            return ??? // is in the ebnf file but not in the original parser..?
        *)
        | _ ->
            return! matchToken IntLiteralToken |> map NumberExpressionSyntax
    }
    |> fun primaryExpressionParser ->
        par {
            let! expression = primaryExpressionParser
            let! leftRecursion = many tryParseOneLeftRecursiveTerm
            return
                leftRecursion
                |> List.fold (fun exp lRec ->
                    match lRec with
                    | FunctionCallLeftRecursion (openParens, args, closeParens) ->
                        FunctionCallExpression {
                            functionExpression = exp
                            openParenthesis = openParens
                            functionCallArguments = args
                            closeParenthesis = closeParens
                        }
                ) expression
        }

and parsePattern : Parser<PatternSyntax> =
    matchToken IdentifierToken |> map VariablePattern

and parseIdentifierWithType : Parser<IdentifierWithType> =
    pipe3
        (matchToken IdentifierToken)
        (matchToken ColonToken)
        (parseTypeExpression)
        (fun identifier colon typeExpression ->
            {
                identifier = identifier
                colon = colon
                requiredType = typeExpression
            }
        )

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
    let (_noToken, initialState) = nextToken (lexer, ParserState(noToken, [], []))
    let (result, _) = syntaxTree (lexer, initialState)
    result