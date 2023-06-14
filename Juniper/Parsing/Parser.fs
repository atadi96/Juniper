module Parsing.Parser

open Tokens
open Lexer

type ParseError = FParsec.Position * FParsec.Position * string

type SeparatedSyntaxList<'TItem> =
    | EmptySyntaxList
    | SeparatedSyntaxList of 'TItem * (Token * 'TItem) list

type ModuleDefinitionSyntax =
    {
        moduleName: ModuleNameSyntax
        openModules: OpenModulesSyntax option
    }
and ModuleNameSyntax =
    {
        moduleKeyword: Token
        moduleNameIdentifier: Token
    }
and OpenModulesSyntax =
    {
        openKeyword: Token
        openParenthesis: Token
        openedModuleNameIdentifiers: SeparatedSyntaxList<Token>
        closeParenthesis: Token
    }

type SyntaxNode =
    | ExpressionSyntax of ExpressionSyntax

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
                let newError = (currentToken.start, currentToken.start, sprintf "unexpected token <%A>, expecting <%A>" currentToken.tokenKind tokenKind)
                matchedToken, ParserState (currentToken, newError :: errors)

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
            par {
                let! condResult = cond
                if condResult then
                    let! current = body
                    let! rest = inner x
                    return current :: rest
                else
                    return []
            }
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
    and parseModuleName : Parser<ModuleNameSyntax> =
        par {
            let! moduleKeyword = matchToken (KeywordToken ModuleKeyword)
            let! moduleName = matchToken (IdentifierToken)
            return {
                moduleKeyword = moduleKeyword
                moduleNameIdentifier = moduleName
            }
        }
    and parseOpenModules : Parser<OpenModulesSyntax> =
        par {
            let! openKeyword = matchToken (KeywordToken OpenKeyword)
            let! openParenthesis = matchToken OpenParenthesisToken
            let! moduleNames = manySep (matchToken IdentifierToken) CommaToken CloseParenthesisToken
            let! closeParenthesis = matchToken CloseParenthesisToken
            return {
                openKeyword = openKeyword
                openParenthesis = openParenthesis
                openedModuleNameIdentifiers = moduleNames
                closeParenthesis = closeParenthesis
            }
        }
    and parseModuleDefinition =
        par {
            let! moduleName = parseModuleName
            let! openModules =
                parseOpenModules
                |> ifCurrentKind (KeywordToken OpenKeyword)
            return {
                moduleName = moduleName
                openModules = openModules
            }
        }
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
        