module Parsing.Parser

open Tokens
open Lexer
open SyntaxTree

type ParserState = ParserState of Token * (ParseError list) * Token list

type Parser<'a> = ((Lexer * ParserState) -> 'a * ParserState)

let current : Parser<Token> = fun (_lexer,state) ->
    let (ParserState(currentToken, _, _)) = state 
    currentToken, state

let private convertTokenToTrivia (token: Token) =
    seq {
        yield! token.leadingTrivia
        yield {
            triviaKind = BadTokenTrivia
            startPos = token.start
            endPos = token.end_
            text = token.text |> Option.defaultValue "" // TODO check if SyntaxTrivia.text should be nullable?
        }
        yield! token.trailingTrivia
    }

let errors : Parser<ParseError list * (FParsec.Position * string) list> =
    fun (lexer, state) ->
        let lexErrors = lexer.GetErrors()
        let (ParserState (_, errors, _)) = state
        (errors |> List.rev, lexErrors), state

let rec nextToken : Parser<Token> =
    fun (lexer, state) ->
        let rec getNextGoodToken accumulatedSkippedTokens =
            let newToken = lexer.Lex()
            if newToken.tokenKind = BadToken then
                getNextGoodToken (newToken :: accumulatedSkippedTokens)
            else
                let newToken =
                    match accumulatedSkippedTokens with
                    | [] -> newToken
                    | skippedTokens ->
                        let leadingTrivia =
                            skippedTokens
                            |> Seq.rev
                            |> Seq.collect convertTokenToTrivia
                            |> Seq.toList
                        { newToken with
                            leadingTrivia = leadingTrivia @ newToken.leadingTrivia
                        }
                let (ParserState(currentToken, errors, _)) = state
                currentToken, ParserState (newToken, errors, [])

        let (ParserState (_, _, skippedTokens)) = state
        getNextGoodToken skippedTokens
        
let skipSilent : Parser<unit> =
    fun (lexer, ParserState(currentToken, errors, skippedTokens)) ->
        let (_currentToken, state) =
            nextToken (lexer, ParserState(currentToken, errors, currentToken :: skippedTokens))
        (), state

let matchToken (tokenKind: TokenKind) : Parser<Token> =
    fun (lexer, state) ->
        let (ParserState (currentToken, _, _)) = state
        if currentToken.tokenKind = tokenKind then
            nextToken (lexer, state)
        else
            let (ParserState (_, errors, skippedTokens)) = state
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
            let newError = PE (currentToken.start |> Position.toFParsec, currentToken.end_ |> Position.toFParsec, sprintf "Unexpected token <%A>, expecting <%A>" currentToken.tokenKind tokenKind, [])
            let newErrors =
                match errors with
                | PE (startPos, endPos, text, children) :: rest when startPos = (currentToken.start |> Position.toFParsec) && endPos = (currentToken.end_ |> Position.toFParsec) ->
                    PE (startPos, endPos, text, newError :: children) :: rest
                | _ -> newError :: errors
                        
            matchedToken, ParserState (currentToken, newErrors, skippedTokens)

let customError start end_ text: Parser<unit> =
    fun (_lexer,ParserState (currentToken, errors, skippedTokens)) ->
        let newError = PE (start, end_, text, [])
        let newErrors = newError :: errors
        (), ParserState (currentToken, newErrors, skippedTokens)

let map f (par: Parser<_>): Parser<_> =
    fun (lexer, state) ->
        let (x, state') = par (lexer,state)
        f x, state'

let bind (f: _ -> Parser<_>) (par: Parser<_>) =
    fun (lexer, state) ->
        let (x, state') = par (lexer,state)
        f x (lexer, state')

let ret x: Parser<_> = fun (_lexer, state) -> x, state

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

type ParBuilder() =
    member __.Bind(x,f) = bind f x
    member __.Bind2(a,b,binder) =
        pipe2 a b binder
    member __.Bind3(a,b,c,binder) =
        pipe3 a b c binder
    member __.Bind4(a,b,c,d,binder) =
        pipe4 a b c d binder
    member __.Bind5(a,b,c,d,e,binder) =
        pipe5 a b c d e binder
    member __.Bind6(a,b,c,d,e,f,binder) =
        pipe6 a b c d e f binder
    member __.Return x = ret x
    member __.Zero() = ret ()
    member __.ReturnFrom x = x
    member __.Do x = x |> map ignore

let par = ParBuilder()

let currentKind : Parser<TokenKind> =
    current
    |> map (fun x -> x.tokenKind)
            
let many (item: Parser<_ option>): Parser<_ list> =
    let rec manyInner (lexer, state) =
        let (current, state') = item (lexer,state)
        match current with
        | None -> [], state'
        | Some parsedCurrent ->
            let (rest, state'') = manyInner (lexer, state')
            parsedCurrent :: rest, state''

    manyInner
        
            
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

let rec choice parsers: Parser<_> =
    match parsers with
    | [] -> ret None
    | p :: ps ->
        par {
            match! p with
            | Some x ->
                return Some x
            | None ->
                return! choice ps
        }
    
let peek1 : Parser<Token> =
    fun (lexer,state) ->
        let peekedToken = lexer.Peek1()
        (peekedToken, state)