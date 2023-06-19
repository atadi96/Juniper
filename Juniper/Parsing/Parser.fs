module Parsing.Parser

open Tokens
open Lexer
open SyntaxTree

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
        let rec getNextGoodToken accumulatedLeadingTrivia =
            let newToken = lexer.Lex()
            if newToken.tokenKind = BadToken then
                let currentTrivia =
                    seq {
                        yield! newToken.leadingTrivia
                        yield {
                            triviaKind = BadTokenTrivia
                            startPos = newToken.start
                            endPos = newToken.end_
                            text = newToken.text |> Option.defaultValue "" // TODO check if SyntaxTrivia.text should be nullable?
                        }
                        yield! newToken.trailingTrivia
                    }
                getNextGoodToken (Seq.concat [accumulatedLeadingTrivia; currentTrivia])
            else
                let newToken =
                    match (accumulatedLeadingTrivia |> List.ofSeq) with
                    | [] -> newToken
                    | leadingTrivia ->
                        { newToken with
                            leadingTrivia = leadingTrivia @ newToken.leadingTrivia
                        }
                let (ParserState(currentToken, errors)) = state
                currentToken, ParserState (newToken, errors)
        getNextGoodToken []

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
                    leadingTrivia = []
                    trailingTrivia = []
                }
            let newError = PE (currentToken.start, currentToken.end_, sprintf "Unexpected token <%A>, expecting <%A>" currentToken.tokenKind tokenKind, [])
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
    member __.Do x = x |> map ignore

let par = ParBuilder()

let lexerMode mode (parser: Parser<_>): Parser<_> =
    fun (lexer, state) ->
        let initialMode = lexer.CurrentMode()
        lexer.SetMode mode
        let result = parser (lexer, state)
        lexer.SetMode initialMode
        result

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
        
let many1Sep (item: Parser<_>) (sepToken): Parser<SeparatedNonEmptySyntaxList<_>> =
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
        let! firstItem = item
        let! sepItems = nextItemParser
        return SeparatedNonEmptySyntaxList (firstItem, sepItems)
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
    
let peek1 : Parser<Token> =
    fun (lexer,state) ->
        let peekedToken = lexer.Peek1()
        (peekedToken, state)