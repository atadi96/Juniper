module Parsing.Parser

open Tokens
open Lexer

type ParseError = PE of (FParsec.Position * FParsec.Position * string * (ParseError list))

type SeparatedSyntaxList<'TItem> =
    | EmptySyntaxList
    | SeparatedSyntaxList of 'TItem * (Token * 'TItem) list

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
    

type SyntaxTree =
    {
        lexErrors : (FParsec.Position * string) list
        parseErrors: ParseError list
        expression: ModuleDefinitionSyntax
        eofToken: Token
    }

module Syntax =

    type ParserState = ParserState of Token * (ParseError list)

    type Parser<'a> = ((Lexer * ParserState) -> 'a * ParserState)

    let current : Parser<Token> = fun (_lexer,state) ->
        let (ParserState(currentToken, _)) = state 
        currentToken, state

    let errors : Parser<ParseError list * (FParsec.Position * string) list> =
        fun (lexer, state) ->
            let lexErrors = lexer.GetErrors()
            let (ParserState (_, errors)) = state
            (errors |> List.rev, lexErrors), state
    
    let rec nextToken : Parser<Token> =
        fun (lexer, state) ->
            let newToken = lexer.Lex()
            if newToken.tokenKind = BadToken then
                nextToken (lexer, state)
            else
                let (ParserState(currentToken, errors)) = state
                currentToken, ParserState (newToken, errors)

    let matchToken (tokenKind: TokenKind) : Parser<Token> =
        fun (lexer, state) ->
            let (ParserState (currentToken, _)) = state
            if currentToken.tokenKind = tokenKind then
                nextToken (lexer, state)
            else
                let (ParserState (_, errors)) = state
                let matchedToken =
                    {
                        start = currentToken.start
                        end_ = currentToken.start
                        value = None
                        text = None
                        tokenKind = tokenKind
                    }
                let newError = PE (currentToken.start, currentToken.end_, sprintf "unexpected token <%A>, expecting <%A>" currentToken.tokenKind tokenKind, [])
                let newErrors =
                    match errors with
                    | PE (startPos, endPos, text, children) :: rest when startPos = currentToken.start && endPos = currentToken.end_ ->
                        PE (startPos, endPos, text, newError :: children) :: rest
                    | _ -> newError :: errors
                        
                matchedToken, ParserState (currentToken, newErrors)

    let map f (par: Parser<_>): Parser<_> =
        fun (lexer, state) ->
            let (x, state') = par (lexer,state)
            f x, state'

    let bind (f: _ -> Parser<_>) (par: Parser<_>) =
        fun (lexer, state) ->
            let (x, state') = par (lexer,state)
            f x (lexer, state')

    let ret x: Parser<_> = fun (_lexer, state) -> x, state

    type ParBuilder() =
        member __.Bind(x,f) = bind f x
        member __.Return x = ret x
        member __.Zero() = ret ()
        member __.ReturnFrom x = x

    let par = ParBuilder()

    let pipe2 (p1: Parser<_>) (p2: Parser<_>) f : Parser<_> = fun (lexer,state) ->
        let (x, state') = p1 (lexer,state)
        let (y, state'') = p2 (lexer, state')
        f x y, state''
        
    let pipe3 (p1: Parser<_>) (p2: Parser<_>) (p3 : Parser<_>) f : Parser<_> = fun (lexer,state) ->
        let (x, state) = p1 (lexer,state)
        let (y, state) = p2 (lexer, state)
        let (z, state) = p3 (lexer, state)
        f x y z, state

    let pipe4 (p1: Parser<_>) (p2: Parser<_>) (p3 : Parser<_>) (p4 : Parser<_>) f : Parser<_> = fun (lexer,state) ->
        let (a, state) = p1 (lexer,state)
        let (b, state) = p2 (lexer, state)
        let (c, state) = p3 (lexer, state)
        let (d, state) = p4 (lexer, state)
        f a b c d, state
    
    let pipe5 (p1: Parser<_>) (p2: Parser<_>) (p3 : Parser<_>) (p4 : Parser<_>) (p5 : Parser<_>) f : Parser<_> = fun (lexer,state) ->
        let (a, state) = p1 (lexer,state)
        let (b, state) = p2 (lexer, state)
        let (c, state) = p3 (lexer, state)
        let (d, state) = p4 (lexer, state)
        let (e, state) = p5 (lexer, state)
        f a b c d e, state

    let pipe6 (p1: Parser<_>) (p2: Parser<_>) (p3 : Parser<_>) (p4 : Parser<_>) (p5 : Parser<_>) (p6 : Parser<_>) f : Parser<_> = fun (lexer,state) ->
        let (a, state) = p1 (lexer,state)
        let (b, state) = p2 (lexer, state)
        let (c, state) = p3 (lexer, state)
        let (d, state) = p4 (lexer, state)
        let (e, state) = p5 (lexer, state)
        let (g, state) = p6 (lexer, state)
        f a b c d e g, state

    let currentKind : Parser<TokenKind> =
        current
        |> bind (fun x -> ret x.tokenKind)
            
            
    let manySep (item: Parser<_>) (sepToken) (closeToken): Parser<SeparatedSyntaxList<_>> =
        let rec nextItemParser (lexer: Lexer,parserState : ParserState) = 
            sepItemParser (lexer,parserState)
        and sepItemParser =
            par {
                let! currentTokenKind = currentKind
                if currentTokenKind = sepToken then
                    let! sepToken = nextToken
                    let! currentItem = item
                    let! rest = nextItemParser
                    return (sepToken, currentItem) :: rest
                else
                    return []
            }
        par {
            let! currentTokenKind = currentKind
            if currentTokenKind = closeToken then
                return EmptySyntaxList
            else
                let! firstItem = item
                let! sepItems = nextItemParser
                return SeparatedSyntaxList (firstItem, sepItems)
        }

    let manyWhile (cond: Parser<bool>) (body: Parser<_>) =
        let rec inner x =
            (par {
                let! condResult = cond
                if condResult then
                    let! current = body
                    let! rest = inner
                    return current :: rest
                else
                    return []
            }) x
        inner

    let ifCurrentKind peekKind parser =
        par {
            let! kind = currentKind
            if kind = peekKind then
                let! parsed = parser
                return Some parsed
            else
                return None
        }

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
            //let! expr = expression
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
        