module Parsing.Syntax

open Parsing.Parser
open SyntaxTree
open Tokens
open Lexer



module rec CapacityExpressions =
    open SyntaxTree.CapacityExpressions

    let parseCapacityExpression : Parser<CapacityExpressionSyntax> =
        par {
            match! currentKind with
            // | statements?
            | _ ->
                return! parseBinaryCapacityExpression 0
        }

    let private parseBinaryCapacityExpression (parentPrecedence: int) =
        let rec binaryRight left =
            par {
                match! currentKind |> map SyntaxFacts.getBinaryCapacityOperatorPrecedence with
                | currentPrecedence when currentPrecedence > parentPrecedence ->
                    let! operatorToken = nextToken
                    let! right = parseBinaryCapacityExpression currentPrecedence
                    return! binaryRight (BinaryCapacityExpression (left, operatorToken, right))
                | _ -> return left
            }

        par {
            let! unaryOperatorPrecedence = currentKind |> map SyntaxFacts.getUnaryCapacityOperatorPrecedence
            let! left =
                if unaryOperatorPrecedence <> 0 && unaryOperatorPrecedence > parentPrecedence then
                    pipe2
                        nextToken
                        (parseBinaryCapacityExpression unaryOperatorPrecedence)
                        (fun operator operand -> failwith "shouldn't happen: no unary Capacity operators in the syntax")
                else
                    parsePrimaryCapacityExpression
            return! binaryRight left
        }

    let private parsePrimaryCapacityExpression : Parser<CapacityExpressionSyntax> =
        lexerMode TypeMode (par {
            match! currentKind with
            | IdentifierToken ->
                let! identifier = nextToken
                return IdentifierCapacityExpression identifier
            | _ ->
                let! number = matchToken NaturalNumberToken
                return NaturalNumberCapacityExpression number
        })

module rec Types =
    open SyntaxTree.Types

    let private parseClosureTypeExpression : Parser<ClosureTypeExpressionSyntax> =
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

    let private parseFunctionTypeExpressionStartingFromArguments : Parser<Token * ClosureOfFunctionSyntax * Token -> FunctionTypeExpressionSyntax> =
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

    type private TypeExpressionLeftRecursion =
        | CapacityLeftRecursion of (Token * CapacityExpressions.CapacityExpressionSyntax * Token)
        | RefLeftRecursion of (Token)
        
    let private tryParseOneTypeExpressionLeftRecursion =
        par {
            match! currentKind with
            | OpenBracketToken ->
                let! openBracket = nextToken
                let! capacity = CapacityExpressions.parseCapacityExpression
                let! closeBracket = matchToken CloseBracketToken
                let leftRecursion = CapacityLeftRecursion (openBracket, capacity, closeBracket)
                return Some leftRecursion
            | (KeywordToken RefKeyword) ->
                let! refKeyword = nextToken
                let leftRecursion = RefLeftRecursion refKeyword
                return Some leftRecursion
            | _ ->
                return None
        }

    let parseTypeExpression : Parser<TypeExpressionSyntax> =
        lexerMode TypeMode (par {
            match! currentKind with
            | IdentifierToken ->
                return! parseDeclarationReferenceWithOptionalTemplateApplication |> map DeclarationReferenceTypeExpression
            | TypeVariableIdentifierToken ->
                return! nextToken |> map TypeVariableIdentifierTypeExpression
            | PipeToken ->
                return! parseClosureTypeExpression |> map ClosureTypeExpression
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
                        (openParenthesis, ClosureTypeExpressionOfFunction closureType, closeParenthesis)
                        |> restOfFunctionType
                        |> FunctionTypeExpression
                | _ ->
                    // (
                    //  ^
                    // which is either parenthesized type expression or ('closure)(...) -> <ty-expr> case of function types
                    let! innerType = parseTypeExpression // TODO for tuples, this has to become a list
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
                            (openParenthesis, ClosureTypeVariableOfFunction typeVariable, closeParenthesis)
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
                                |> map SeparatedNonEmptySyntaxList.toSeparatedSyntaxList
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
        |> fun parsePrimaryTypeExpression ->
            par {
                let! primaryTypeExpression = parsePrimaryTypeExpression
                let! leftRecursion = many tryParseOneTypeExpressionLeftRecursion
                return
                    leftRecursion
                    |> List.fold (fun foldingTypeExpression lRec ->
                        match lRec with
                        | CapacityLeftRecursion (openBracket, capacity, closeBracket) ->
                            CapacityTypeExpression {
                                typeExpression = foldingTypeExpression
                                openBracket = openBracket
                                capacityExpression = capacity
                                closeBracket = closeBracket
                            }
                        | RefLeftRecursion (refKeyword) ->
                            RefTypeExpression {
                                typeExpression = foldingTypeExpression
                                refKeyword = refKeyword
                            }
                    ) primaryTypeExpression
            }
        |> fun parseTypeExceptTuple ->
            par {
                match! many1Sep parseTypeExceptTuple StarToken with
                | SeparatedNonEmptySyntaxList (singleItem, []) -> return singleItem
                | multipleItems -> return TupleTypeExpression multipleItems
            }


    let parseIdentifierWithType : Parser<IdentifierWithType> =
        par {
            let! identifier = matchToken IdentifierToken
            return! lexerMode TypeMode (par {
                let! colon = matchToken ColonToken
                let! typeExpression = Types.parseTypeExpression
                return {
                    identifier = identifier
                    colon = colon
                    requiredType = typeExpression
                }
            })
        }

    let parseOptionalType : Parser<(Token * TypeExpressionSyntax) option> =
        lexerMode TypeMode (
            pipe2
                (matchToken ColonToken)
                Types.parseTypeExpression
                (fun colon typeExpression -> colon, typeExpression)
            |> ifCurrentKind ColonToken
        )
        
    let parseIdentifierWithOptionalType : Parser<IdentifierWithOptionalType> =
        par {
            let! identifier = matchToken IdentifierToken
            let! optionalType = parseOptionalType
            return {
                identifier = identifier
                optionalType = optionalType
            }
        }

    let parseTemplateApplication : Parser<Types.TemplateApplicationSyntax> =
        let parseTemplateApplicationCapacityExpressions =
            lexerMode TypeMode (par {
                let! semicolon = matchToken SemicolonToken
                let! capacityExpressions = many1Sep CapacityExpressions.parseCapacityExpression CommaToken
                return {
                    semicolon = semicolon
                    capacities = capacityExpressions
                }
            })
        lexerMode TypeMode (par {
            let! lessThan = matchToken LessThanToken
            let! templateApplicationTypes = many1Sep Types.parseTypeExpression CommaToken
            let! optionalCapacities =
                parseTemplateApplicationCapacityExpressions
                |> ifCurrentKind SemicolonToken
            let! greaterThan = matchToken GreaterThanToken
            return {
                lessThanSign = lessThan
                templateApplicationTypes = templateApplicationTypes
                optionalCapacityExpressions = optionalCapacities
                greaterThanSign = greaterThan
            }
        })

    let parseDeclarationReferenceWithOptionalTemplateApplication : Parser<DeclarationReferenceSyntax * (Types.TemplateApplicationSyntax option)> =
        par {
            let! firstIdentifier = nextToken
            let! currentToken = current
            match! ret (firstIdentifier, currentToken) with
            // all cases we have to cover:
            // module:member[<...>]
            // id[<...>]
            // cases which we only parse partially:
            // id: ty
            //   ^
            // id :ty
            //    ^
            // id : ty
            //    ^
            // id <
            //    ^
            // module:member <
            //               ^
            | ({ trailingTrivia = []}, { tokenKind = ColonToken; trailingTrivia = [] }) ->
                // moduleName:_
                //           ^
                let! colon = nextToken
                let! moduleNameDeclarationIDentifier = matchToken IdentifierToken
                let! currentToken = current
                let! optionalTemplateApply =
                    par {
                        match! ret (moduleNameDeclarationIDentifier, currentToken) with
                        | ({ trailingTrivia = [] }, { tokenKind = LessThanToken }) ->
                            // moduleName:member<
                            //                  ^
                            let! templateApplication = parseTemplateApplication
                            return Some templateApplication
                        | _ ->
                            //moduleName:member(**)
                            //                     ^
                            //moduleName:member <
                            //                  ^
                            //moduleName:member _
                            //                  ^
                            return None
                    }
                let moduleQualifier = ModuleQualifierDeclarationReference {
                    moduleNameIdentifier = firstIdentifier
                    colon = colon
                    moduleDeclarationNameIdentifier = moduleNameDeclarationIDentifier
                }
                return (moduleQualifier, optionalTemplateApply)
            | ({ trailingTrivia = [] }, { tokenKind = LessThanToken }) ->
                // identifier<
                //           ^
                let! templateApplication = parseTemplateApplication
                return (IdentifierDeclarationReference { declarationNameIdentifier = firstIdentifier } , Some templateApplication)
            | _ ->
                // identifier(**)
                //               ^
                // identifier <
                //            ^
                // identifier: _
                //           ^
                // identifier :
                //            ^
                // identifier _
                //            ^
                return (IdentifierDeclarationReference { declarationNameIdentifier = firstIdentifier } , None)
        }

    let private parseConstraint : Parser<ConstraintSyntax> =
        lexerMode TypeMode (par {
            match! currentKind with
            | KeywordToken NumKeyword ->
                return! nextToken |> map NumConstraint
            | KeywordToken IntKeyword ->
                return! nextToken |> map IntConstraint
            | KeywordToken RealKeyword ->
                return! nextToken |> map RealConstraint
            | KeywordToken PackedKeyword ->
                return! nextToken |> map PackedConstraint
            | OpenBraceToken ->
                let! openBrace = nextToken
                let! memberConstraints = many1Sep parseIdentifierWithType SemicolonToken
                let! closeBrace = nextToken
                return MemberConstraint {
                    openBrace = openBrace
                    fieldConstraints = memberConstraints
                    closeBrace = closeBrace
                }
            | _ ->
                let! num = matchToken (KeywordToken NumKeyword)
                do! skipSilent
                return NumConstraint num
        })

    let private parseTypeConstraint : Parser<TypeConstraintSyntax> =
        par {
            let! typeExpression = parseTypeExpression
            let! colon = matchToken ColonToken
            let! constraint = parseConstraint
            return {
                typeExpression = typeExpression
                colon = colon
                constraint = constraint
            }
        }

    let parseWhereConstraints : Parser<WhereConstraintsSyntax> =
        lexerMode TypeMode (par {
            let! whereKeyword = matchToken (KeywordToken WhereKeyword)
            let! typeConstraints = many1Sep parseTypeConstraint CommaToken
            return {
                whereKeyword = whereKeyword
                typeConstraints = typeConstraints
            }
        })

module Patterns =
    open SyntaxTree.Patterns

    let rec parsePattern : Parser<PatternSyntax> =
        par {
            match! currentKind with
            | KeywordToken MutableKeyword ->
                let! mutableKeyword = nextToken
                let! identifier = Types.parseIdentifierWithOptionalType
                return VariablePattern {
                    mutableKeyword = Some mutableKeyword
                    variablePatternIdentifier = identifier.identifier
                    optionalType = identifier.optionalType
                }
            | KeywordToken TrueKeyword ->
                return! nextToken |> map TruePattern
            | KeywordToken FalseKeyword ->
                return! nextToken |> map FalsePattern
            | IntLiteralToken ->
                return! nextToken |> map IntegerPattern
            | IdentifierToken ->
                let! firstIdentifier = nextToken
                let! currentToken = current
                match! ret (firstIdentifier, currentToken) with
                // all cases we have to cover:
                // module:member[<...>](...)
                // id<...>(...)
                // id(...)
                // id: ty
                // id :ty
                // id : ty
                // _
                // id
                | ({ trailingTrivia = []}, { tokenKind = ColonToken; trailingTrivia = [] }) ->
                    // moduleName:
                    //           ^
                    let! colon = nextToken
                    let! moduleMember = matchToken IdentifierToken
                    let! restOfValCon = parseValConRest
                    return
                        ModuleQualifierDeclarationReference {
                            moduleNameIdentifier = firstIdentifier
                            colon = colon
                            moduleDeclarationNameIdentifier = moduleMember
                        }
                        |> restOfValCon
                | (_, { tokenKind = LessThanToken })
                | (_, { tokenKind = OpenParenthesisToken }) ->
                    // id<
                    //   ^
                    // -or-
                    // id(
                    //   ^
                    let! restOfValcon = parseValConRest
                    return
                        IdentifierDeclarationReference {
                            declarationNameIdentifier = firstIdentifier
                        }
                        |> restOfValcon
                | (_, { tokenKind = ColonToken }) ->
                    // id: ty
                    //   ^
                    // -or-
                    // id : ty
                    //    ^
                    // -or-
                    // id :ty
                    //    ^
                    let! optionalType = Types.parseOptionalType
                    return VariablePattern {
                        mutableKeyword = None
                        variablePatternIdentifier = firstIdentifier
                        optionalType = optionalType
                    }
                | ({ text = Some "_"}, _) ->
                    // _
                    //  ^
                    return UnderscorePattern firstIdentifier
                | _ ->
                    return VariablePattern {
                        mutableKeyword = None
                        variablePatternIdentifier = firstIdentifier
                        optionalType = None
                    }
            | OpenParenthesisToken ->
                let! openParenthesis = nextToken
                match! currentKind with
                | CloseParenthesisToken ->
                    let! closeParenthesis = matchToken CloseParenthesisToken
                    return UnitPattern (openParenthesis, closeParenthesis)
                | _ ->
                    let! firstPattern = parsePattern
                    match! currentKind with
                    | CloseParenthesisToken ->
                        let! closeParenthesis = nextToken
                        return ParenthesizedPattern {
                            openParenthesis = openParenthesis
                            pattern = firstPattern
                            closeParenthesis = closeParenthesis
                        }
                    | _ ->
                        let! firstComma = matchToken CommaToken
                        let! followingPatterns = many1Sep parsePattern CommaToken
                        let allPatterns =
                            followingPatterns
                            |> SeparatedNonEmptySyntaxList.prepend (firstPattern, firstComma)
                        let! closeParenthesis = matchToken CloseParenthesisToken
                        return TuplePattern {
                            openParenthesis = openParenthesis
                            patterns = allPatterns
                            closeParenthesis = closeParenthesis
                        }
            | OpenBraceToken ->
                let! openBrace = matchToken OpenBraceToken
                let! fieldPatterns = manySep parseFieldPattern SemicolonToken CloseBraceToken
                let! closeBrace = matchToken CloseBraceToken
                return RecordPattern {
                    openBrace = openBrace
                    fieldPatterns = fieldPatterns
                    closeBrace = closeBrace
                }
            | _ ->
                let! identifier = matchToken IdentifierToken
                return VariablePattern {
                    mutableKeyword = None
                    variablePatternIdentifier = identifier
                    optionalType = None
                }
        }

    and parseValConRest : Parser<DeclarationReferenceSyntax -> PatternSyntax> =
        par {
            let! optionalTemplateApplication =
                Types.parseTemplateApplication
                |> ifCurrentKind LessThanToken
            let! openParenthesis = matchToken OpenParenthesisToken
            let! valConArguments =
                manySep parsePattern CommaToken CloseParenthesisToken
            let! closeParenthesis = matchToken CloseParenthesisToken
            return fun declarationReference ->
                ValConPattern {
                    valConDeclarationReference = declarationReference
                    optionalTemplateApplication = optionalTemplateApplication
                    openParenthesis = openParenthesis
                    valConArguments = valConArguments
                    closeParenthesis = closeParenthesis
                }
        }
    
    
    and parseFieldPattern: Parser<FieldPatternSyntax> =
        par {
            let! fieldIdentifier = matchToken IdentifierToken
            let! equal = matchToken EqualsToken
            let! fieldPattern = parsePattern
            return {
                fieldIdentifier = fieldIdentifier
                equal = equal
                fieldPattern = fieldPattern
            }
        }
    
module rec Expressions =
    open SyntaxTree.Expressions

    type private ExpressionLeftRecursion =
        | FunctionCallLeftRecursion of Token * SeparatedSyntaxList<ExpressionSyntax> * Token
        | IndexerLeftRecursion of (Token * ExpressionSyntax * Token)
        | MemberAccessLeftRecursion of (Token * Token)
        | TypeLeftRecursion of (Token * Types.TypeExpressionSyntax)

    let parseExpression : Parser<ExpressionSyntax> =
        par {
            match! currentKind with
            // | statements?
            | _ ->
                return! parseBinaryExpression 0
        }

    let private parseBinaryExpression (parentPrecedence: int) =
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

    let parseFieldAssign =
        par {
            let! fieldNameIdentifier = matchToken IdentifierToken
            let! equals = matchToken EqualsToken
            let! expression = parseExpression
            return {
                fieldNameIdentifier = fieldNameIdentifier
                equals = equals
                expression = expression
            }
        }

    let private parsePrimaryExpression : Parser<ExpressionSyntax> =
        let tryParseOneLeftRecursiveTerm: Parser<ExpressionLeftRecursion option> =
            par {
                match! currentKind with
                | OpenParenthesisToken ->
                    let! openParenthesis = nextToken
                    let! arguments = manySep parseExpression CommaToken CloseParenthesisToken
                    let! closeParenthesis = matchToken CloseParenthesisToken
                    let leftRecursion = FunctionCallLeftRecursion (openParenthesis, arguments, closeParenthesis)
                    return Some leftRecursion
                | OpenBracketToken ->
                    let! openBracket = nextToken
                    let! indexExpression = parseExpression
                    let! closeBracket = matchToken CloseBracketToken
                    let leftRecursion = IndexerLeftRecursion (openBracket, indexExpression, closeBracket)
                    return Some leftRecursion
                | DotToken ->
                    let! dot = nextToken
                    let! identifier = matchToken IdentifierToken
                    let leftRecursion = MemberAccessLeftRecursion (dot, identifier)
                    return Some leftRecursion
                | ColonToken ->
                    return! lexerMode TypeMode (par {
                        let! colon = nextToken
                        let! typeExpression = Types.parseTypeExpression
                        let leftRecursion = TypeLeftRecursion (colon, typeExpression)
                        return Some leftRecursion
                        
                    })
                | _ ->
                    return None
            }
        par {
            match! currentKind with
            | IdentifierToken ->
                return! Types.parseDeclarationReferenceWithOptionalTemplateApplication |> map DeclarationReferenceExpressionSyntax
            | KeywordToken TrueKeyword
            | KeywordToken FalseKeyword ->
                return! nextToken |> map BoolLiteralExpression
            | KeywordToken NullKeyword ->
                return! nextToken |> map NullLiteralExpression
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
                            |> SeparatedNonEmptySyntaxList.prepend (innerExpression, firstSemicolon)
                        let! closeParenthesis = matchToken CloseParenthesisToken
                        return SequenceExpression(openParenthesis, sequence, closeParenthesis)
                    | CommaToken ->
                        let! firstComma = nextToken
                        let! rest = many1Sep parseExpression CommaToken
                        let tuple =
                            rest
                            |> SeparatedNonEmptySyntaxList.prepend (innerExpression, firstComma)
                        let! closeParenthesis = matchToken CloseParenthesisToken
                        return TupleExpression(openParenthesis, tuple, closeParenthesis)
                    | CloseParenthesisToken
                    | _ ->
                        let! closeParenthesis = matchToken CloseParenthesisToken
                        return ParenthesizedExpressionSyntax (openParenthesis, innerExpression, closeParenthesis)
            | InlineCppToken ->
                return! matchToken InlineCppToken |> map InlineCppExpressionSyntax
            | KeywordToken LetKeyword ->
                let! letKeyword = nextToken
                let! pattern = Patterns.parsePattern
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
                let! arguments = manySep Types.parseIdentifierWithOptionalType CommaToken CloseParenthesisToken
                let! closeParenthesis = matchToken CloseParenthesisToken
                let! optionalReturnType = Types.parseOptionalType
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
            | KeywordToken CaseKeyword ->
                let! caseKeyword = nextToken
                let! matchExpression = parseExpression
                let! ofKeyword = matchToken (KeywordToken OfKeyword)
                let! firstPipe = matchToken (PipeToken)
                let! clauses = many1Sep parseCaseClause PipeToken
                let! endKeyword = matchToken (KeywordToken EndKeyword)
                return CaseOfExpression {
                    caseKeyword = caseKeyword
                    matchExpression = matchExpression
                    ofKeyword = ofKeyword
                    firstPipe = firstPipe
                    caseClauses = clauses
                    endKeyword = endKeyword
                }
            | KeywordToken IfKeyword ->
                let! ifKeyword = nextToken
                let! conditionExpression = parseExpression
                let! thenKeyword = matchToken (KeywordToken ThenKeyword)
                let! trueExpression = parseExpression
                let! elifBranches = many parseElifBranch
                let! elseKeyword = matchToken (KeywordToken ElseKeyword)
                let! falseExpression = parseExpression
                let! endKeyword = matchToken (KeywordToken EndKeyword)
                return IfExpression {
                    ifKeyword = ifKeyword
                    conditionExpression = conditionExpression
                    thenKeyword = thenKeyword
                    trueExpression = trueExpression
                    elifBranches = elifBranches
                    elseKeyword = elseKeyword
                    falseExpression = falseExpression
                    endKeyword = endKeyword
                }
            | KeywordToken VarKeyword ->
                let! varKeyword = matchToken (KeywordToken VarKeyword)
                let! variableIdentifier = matchToken IdentifierToken
                return! lexerMode TypeMode (par {
                    let! colonToken = matchToken ColonToken
                    let! variableType = Types.parseTypeExpression
                    return VariableDeclaration {
                        varKeyword = varKeyword
                        variableIdentifier = variableIdentifier
                        colonToken = colonToken
                        variableType = variableType
                    }
                })
            | KeywordToken SetKeyword ->
                let! setKeyword = matchToken (KeywordToken SetKeyword)
                let! optionalRefKeyword =
                    nextToken
                    |> ifCurrentKind (KeywordToken RefKeyword)
                let! leftAssign = parseLeftAssign
                let! equalsToken = matchToken EqualsToken
                let! expression = parseExpression
                return SetExpression {
                    setKeyword = setKeyword
                    optionalRefKeyword = optionalRefKeyword
                    leftAssign = leftAssign
                    equalsToken = equalsToken
                    expression = expression
                }
            | KeywordToken ForKeyword ->
                let! forKeyword = matchToken (KeywordToken ForKeyword)
                let! identifier = matchToken IdentifierToken
                let! optionalTyExpr =
                    pipe2
                        (matchToken ColonToken)
                        Types.parseTypeExpression
                        (fun colon tyExpr -> colon, tyExpr)
                    |> ifCurrentKind ColonToken
                    |> lexerMode TypeMode
                let! inKeyword = matchToken (KeywordToken InKeyword)
                let! startExpression = parseExpression
                let! direction =
                    par {
                        match! currentKind with
                        | KeywordToken DownToKeyword ->
                            return! nextToken
                        | _ ->
                            return! matchToken (KeywordToken ToKeyword)
                    }
                let! endExpression = parseExpression
                let! doKeyword = matchToken (KeywordToken DoKeyword)
                let! bodyExpression = parseExpression
                let! endKeyword = matchToken (KeywordToken EndKeyword)
                return ForLoopExpression {
                    forKeyword = forKeyword
                    identifier = identifier
                    optionalTyExpr = optionalTyExpr
                    inKeyword = inKeyword
                    startExpression = startExpression
                    direction = direction
                    endExpression = endExpression
                    doKeyword = doKeyword
                    bodyExpression = bodyExpression
                    endKeyword = endKeyword
                }
            | KeywordToken DoKeyword ->
                let! doKeyword = matchToken (KeywordToken DoKeyword)
                let! bodyExpression = parseExpression
                let! whileKeyword = matchToken (KeywordToken WhileKeyword)
                let! conditionExpression = parseExpression
                let! endKeyword = matchToken (KeywordToken EndKeyword)
                return DoWhileExpression {
                    doKeyword = doKeyword
                    bodyExpression = bodyExpression
                    whileKeyword = whileKeyword
                    conditionExpression = conditionExpression
                    endKeyword = endKeyword
                }
            | KeywordToken WhileKeyword ->
                let! whileKeyword = matchToken (KeywordToken WhileKeyword)
                let! conditionExpression = parseExpression
                let! doKeyword = matchToken (KeywordToken DoKeyword)
                let! bodyExpression = parseExpression
                let! endKeyword = matchToken (KeywordToken EndKeyword)
                return WhileDoExpression {
                    whileKeyword = whileKeyword
                    conditionExpression = conditionExpression
                    doKeyword = doKeyword
                    bodyExpression = bodyExpression
                    endKeyword = endKeyword
                }
            | KeywordToken ArrayKeyword ->
                let! arrayStart =
                    lexerMode TypeMode (par {
                        let! arrayKeyword = matchToken (KeywordToken ArrayKeyword)
                        let! arrayTypeExpression = Types.parseTypeExpression
                        return (arrayKeyword, arrayTypeExpression)
                    })
                let (arrayKeyword, arrayTypeExpression) = arrayStart
                match! currentKind with
                | KeywordToken OfKeyword ->
                    let! ofKeyword = matchToken (KeywordToken OfKeyword)
                    let! initializerExpression = parseExpression
                    let! endKeyword = matchToken (KeywordToken EndKeyword)
                    return ArrayExpression {
                        arrayKeyword = arrayKeyword
                        arrayTypeExpression = arrayTypeExpression
                        optionalInitializerExpression = Some {
                            ofKeyword = ofKeyword
                            initializerExpression = initializerExpression
                        }
                        endKeyword = endKeyword
                    }
                | _ ->
                    let! endKeyword = matchToken (KeywordToken EndKeyword)
                    return ArrayExpression {
                        arrayKeyword = arrayKeyword
                        arrayTypeExpression = arrayTypeExpression
                        optionalInitializerExpression = None
                        endKeyword = endKeyword
                    }
            (*
            | OpenBracketToken ->
                let! openBracket = nextToken
                let! listElements = many1Sep parseExpression CommaToken
                let! closeBracket = matchToken CloseBracketToken
                return ??? // is in the ebnf file but not in the original parser..?
            *)
            | KeywordToken SmartPointerKeyword ->
                let! smartPointerKeyword = nextToken
                let! openParenthesis = matchToken OpenParenthesisToken
                let! valueExpression = parseExpression
                let! comma = matchToken CommaToken
                let! destructorExpression = parseExpression
                let! closeParenthesis = matchToken CloseParenthesisToken
                return SmartPointerExpression {
                    smartPointerKeyword = smartPointerKeyword
                    openParenthesis = openParenthesis
                    valueExpression = valueExpression
                    comma = comma
                    destructorExpression = destructorExpression
                    closeParenthesis = closeParenthesis
                }
            | KeywordToken PackedKeyword
            | OpenBraceToken ->
                let! optionalPackedKeyword =
                    nextToken
                    |> ifCurrentKind (KeywordToken PackedKeyword)
                let! openBrace = matchToken OpenBraceToken
                let! fieldAssigns = many1Sep parseFieldAssign SemicolonToken
                let! closeBrace = matchToken CloseBraceToken
                return RecordExpression {
                    optionalPackedKeyword = optionalPackedKeyword
                    openBrace = openBrace
                    fieldAssigns = fieldAssigns
                    closeBrace = closeBrace
                }
            | _ ->
                return! matchToken IntLiteralToken |> map NumberExpressionSyntax
        }
        |> fun primaryExpressionParser ->
            par {
                let! expression = primaryExpressionParser
                let! leftRecursion = many tryParseOneLeftRecursiveTerm
                return
                    leftRecursion
                    |> List.fold (fun foldingExpression lRec ->
                        match lRec with
                        | FunctionCallLeftRecursion (openParens, args, closeParens) ->
                            FunctionCallExpression {
                                functionExpression = foldingExpression
                                openParenthesis = openParens
                                functionCallArguments = args
                                closeParenthesis = closeParens
                            }
                        | IndexerLeftRecursion (openBracket, indexExpression, closeBracket) ->
                            IndexerExpression {
                                expression = foldingExpression
                                openBracket = openBracket
                                indexExpression = indexExpression
                                closeBracket = closeBracket
                            }
                        | MemberAccessLeftRecursion (dot, identifier) ->
                            MemberAccessExpression {
                                expression = foldingExpression
                                dot = dot
                                identifier = identifier
                            }
                        | TypeLeftRecursion (colon, typeExpression) ->
                            TypedExpression {
                                expression = foldingExpression
                                colon = colon
                                typeExpression = typeExpression
                            }
                    ) expression
            }

    let private parseElifBranch : Parser<ElifBranchSyntax option> =
        par {
            match! currentKind with
            | KeywordToken ElifKeyword ->
                let! elifKeyword = matchToken (KeywordToken ElifKeyword)
                let! conditionExpression = parseExpression
                let! thenKeyword = matchToken (KeywordToken ThenKeyword)
                let! trueExpression = parseExpression
                return Some {
                    elifKeyword = elifKeyword
                    conditionExpression = conditionExpression
                    thenKeyword = thenKeyword
                    trueExpression = trueExpression
                }
            | _ -> return None
        }
        
    let private parseCaseClause : Parser<CaseClauseSyntax> =
        pipe3
            Patterns.parsePattern
            (matchToken DoubleArrowToken)
            parseExpression
            (fun pattern doubleArrow expression ->
                {
                    pattern = pattern
                    doubleArrow = doubleArrow
                    expression = expression
                }
            )
    
    type private LeftAssignLeftRecursion =
        | FieldAccessLeftAssignLeftRecursion of Token * Token
        | IndexerLeftAssignLeftRecursion of Token * ExpressionSyntax * Token

    let private parseLeftAssign : Parser<LeftAssignSyntax> =
        let tryParseOneLeftResursiveLeftAssign =
            par {
                match! currentKind with
                | DotToken ->
                    let! dotToken = nextToken
                    let! fieldIdentifier = matchToken IdentifierToken
                    let fieldAccessLeftRecursion = FieldAccessLeftAssignLeftRecursion (dotToken, fieldIdentifier)
                    return Some fieldAccessLeftRecursion
                | OpenBracketToken ->
                    let! openBracket = nextToken
                    let! indexExpression = Expressions.parseExpression
                    let! closeBracket = matchToken CloseBracketToken
                    let indexerLeftRecursion = IndexerLeftAssignLeftRecursion (openBracket, indexExpression, closeBracket)
                    return Some indexerLeftRecursion
                | _ -> return None
            }
        par {
            match! currentKind with
            | IdentifierToken ->
                let! firstIdentifier = nextToken
                let! currentToken = current
                match! ret (firstIdentifier, currentToken) with
                | ({ trailingTrivia = []}, { tokenKind = ColonToken; trailingTrivia = [] }) ->
                    // moduleName:
                    //           ^ 
                    let! colon = nextToken
                    let! moduleMember = matchToken IdentifierToken
                    return {
                        moduleNameIdentifier = firstIdentifier
                        colon = colon
                        moduleDeclarationNameIdentifier = moduleMember
                    }
                    |> ModuleQualifierDeclarationReference
                    |> DeclarationReferenceLeftAssign
                | _ ->
                    return 
                        { declarationNameIdentifier = firstIdentifier }
                        |> IdentifierDeclarationReference
                        |> DeclarationReferenceLeftAssign
                        
            | _ ->
                let! identifier = matchToken IdentifierToken
                //do! skipSilent // maybe to not get into infinite loop?
                return 
                    { declarationNameIdentifier = identifier }
                    |> IdentifierDeclarationReference
                    |> DeclarationReferenceLeftAssign
        }
        |> fun primaryLeftAssignParser ->
            par {
                let! leftAssign = primaryLeftAssignParser
                let! leftRecursions = many tryParseOneLeftResursiveLeftAssign
                return
                    leftRecursions
                    |> List.fold (fun leftAssign leftRecursion ->
                        match leftRecursion with
                        | FieldAccessLeftAssignLeftRecursion (dotToken, fieldIdentifier) ->
                            RecordMemberLeftAssign {
                                recordLeftAssign = leftAssign
                                memberAccess = dotToken
                                memberIdentifier = fieldIdentifier
                            }
                        | IndexerLeftAssignLeftRecursion (openBracket, indexExpression, closeBracket) ->
                            ArrayAccessLeftAssign {
                                arrayLeftAssign = leftAssign
                                openBracket = openBracket
                                indexExpression = indexExpression
                                closeBracket = closeBracket
                            }
                    ) leftAssign
            }

module Declarations =
    open SyntaxTree.Declarations

    let private parseOpenModules : Parser<OpenModulesSyntax> =
        pipe4
            (matchToken (KeywordToken OpenKeyword))
            (matchToken OpenParenthesisToken)
            (manySep (matchToken IdentifierToken) CommaToken CloseParenthesisToken)
            (matchToken CloseParenthesisToken)
            OpenModulesSyntax.Create

    let private parseTemplateDec : Parser<TemplateDeclarationSyntax> =
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
        let parseCapacityIdentifiers =
            lexerMode TypeMode (par {
                let! semicolon = matchToken SemicolonToken
                let! identifiers = many1Sep (matchToken IdentifierToken) CommaToken
                return {
                    semicolon = semicolon
                    capacityIdentifiers = identifiers
                }
            })
        lexerMode TypeMode (par {
            let! lessThan = matchToken LessThanToken
            let! typeVariables = many1Sep parseTypeVariableIdentifier CommaToken
            let! optionalCapacityIdentifiers =
                parseCapacityIdentifiers
                |> ifCurrentKind SemicolonToken
            let! greaterThan = matchToken GreaterThanToken
            return {
                lessThanSign = lessThan
                typeVariables = typeVariables
                optionalCapacityIdentifiers = optionalCapacityIdentifiers
                greaterThanSign = greaterThan
            }
        })

    let private parseValueConstructor : Parser<ValueConstructorSyntax> =
        par {
            let! valueConstructorIdentifier = matchToken IdentifierToken
            let! openParenthesis = matchToken OpenParenthesisToken
            let! valueConstructorParameterTypes =
                manySep Types.parseTypeExpression CommaToken CloseParenthesisToken
            let! closeParenthesis = matchToken CloseParenthesisToken
            return {
                valueConstructorIdentifier = valueConstructorIdentifier
                openParenthesis = openParenthesis
                valueConstructorParameterTypes = valueConstructorParameterTypes
                closeParenthesis = closeParenthesis
            }
        }

    let private parseAlgebraicType : Parser<AlgebraicTypeSyntax> =
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

    let private parseAlias : Parser<AliasSyntax> =
        par {
            let! aliasKeyword = matchToken (KeywordToken AliasKeyword)
            let! aliasIdentifier = matchToken (IdentifierToken)
            let! optionalTemplateDeclaration =
                parseTemplateDec
                |> ifCurrentKind LessThanToken
            let! equals = matchToken EqualsToken
            let! aliasOfType = Types.parseTypeExpression
            return {
                aliasKeyword = aliasKeyword
                aliasIdentifier = aliasIdentifier
                optionalTemplateDeclaration = optionalTemplateDeclaration
                equals = equals
                aliasOfType = aliasOfType
            }
        }

    let private parseLetDeclaration : Parser<LetDeclarationSyntax> =
        par {
            let! letKeyword = matchToken (KeywordToken LetKeyword)
            let! identifier = matchToken IdentifierToken
            let! optionalType = Types.parseOptionalType
            let! equals = matchToken EqualsToken
            let! body = Expressions.parseExpression
            return {
                letKeyword = letKeyword
                identifier = identifier
                optionalType = optionalType
                equals = equals
                body = body
            }
        }

    let private parseFunctionDeclaration : Parser<FunctionDeclarationSyntax> =
        par {
            let! funKeyword = matchToken (KeywordToken FunKeyword)
            let! identifier = matchToken IdentifierToken
            let! optionalTemplateDeclaration =
                parseTemplateDec
                |> ifCurrentKind LessThanToken
            let! openParenthesis = matchToken OpenParenthesisToken
            let! functionArguments =
                manySep Types.parseIdentifierWithOptionalType CommaToken CloseParenthesisToken
            let! closeParenthesis = matchToken CloseParenthesisToken
            let! optionalType = Types.parseOptionalType
            let! optionalWhereConstraints =
                Types.parseWhereConstraints
                |> ifCurrentKind (KeywordToken WhereKeyword)
            let! equal = matchToken EqualsToken
            let! functionBody = Expressions.parseExpression
            return {
                funKeyword = funKeyword
                identifier = identifier
                optionalTemplateDeclaration = optionalTemplateDeclaration
                openParenthesis = openParenthesis
                functionArguments = functionArguments
                closeParenthesis = closeParenthesis
                optionalType = optionalType
                optionalWhereConstraints = optionalWhereConstraints
                equals = equal
                functionBody = functionBody
            }
        }

    let rec parseDeclaration : Parser<DeclarationSyntax> =
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

module Modules =
    open SyntaxTree.Modules

    let private parseModuleName : Parser<ModuleNameSyntax> =
        par {
            let! moduleKeyword = matchToken (KeywordToken ModuleKeyword)
            let! moduleName = matchToken (IdentifierToken)
            return {
                moduleKeyword = moduleKeyword
                moduleNameIdentifier = moduleName
            }
        }

    let parseModuleDefinition =
        lexerMode ExpressionMode (par {
            let! moduleName = parseModuleName
            let! declarations =
                manyWhile
                    (currentKind |> map ((<>) EndOfFileToken))
                    Declarations.parseDeclaration
            return {
                moduleName = moduleName
                declarations = declarations
            }
        })

let syntaxTree : Parser<SyntaxTree> =
    par {
        let! module_ = Modules.parseModuleDefinition
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