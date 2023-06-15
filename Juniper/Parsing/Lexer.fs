module Parsing.Lexer

open FParsec
open Tokens

type UserState =
   {
        parserErrors: ((FParsec.Position * string) * string * ParserError) list
        current: Reply<Token> option
   } with
   static member Create() = { parserErrors = []; current = None }
   member this.PrependError (stream: CharStream<_>, errorText: string) =
        let error = ParserError(stream.Position, stream.UserState, messageError errorText)
        let errorMsg = error.ToString(stream)
        { this with
            parserErrors = ((stream.Position, errorText), errorMsg, error) :: this.parserErrors
        }
   
and Parser<'t> = Parser<'t, UserState>

module private Lexer =
    type TokenData =
         | TokenKind of TokenKind
         | IntLiteralTokenData of int64
         | BadTokenData of char
         //| InlineCppTokenData of string
         //| StringLiteralTokenData of string
         | IdentifierTokenData of string

    let badToken : Parser<TokenData> =
         fun stream ->
             stream.UserState <-
                 stream
                     .UserState
                     .PrependError(stream, sprintf "unexpected character %c" (stream.Peek()))

             (anyChar |>> BadTokenData) stream
    (*
    let inlineCppToken : Parser<string> =
        let normalCharSnippet = manySatisfy (fun c -> c <> '\\' && c <> '#')
        let escapedChar = pstring "\\" >>. (anyOf "\\#" |>> string)
        between (pstring "#") (pstring "#") ((stringsSepBy normalCharSnippet escapedChar))
    *)

    let private getKeywordOrIdentifierTokenData (text: string) =
        text
        |> SyntaxFacts.getKeyword
        |> Option.map (KeywordToken >> TokenKind)
        |> Option.defaultValue (IdentifierTokenData text)

    let tokenData : Parser<TokenData> =
        choice
          [
            eof >>% TokenKind EndOfFileToken

            skipChar '+' >>% TokenKind PlusToken
            skipChar '-' >>% TokenKind MinusToken
            //skipChar '-' >>. (skipChar '>' >>% TokenKind ArrowToken <|> preturn (TokenKind MinusToken))
            
            skipChar '*' >>% TokenKind StarToken
            skipChar '/' >>% TokenKind SlashToken

            skipChar '(' >>% TokenKind OpenParenthesisToken
            skipChar ')' >>% TokenKind CloseParenthesisToken

            skipChar ',' >>% TokenKind CommaToken
            
            skipChar '=' >>% TokenKind EqualsToken
            //skipChar '=' >>. choice [ skipChar '=' >>% TokenKind EqualsEqualsToken; skipChar '>' >>% TokenKind DoubleArrowToken; preturn (TokenKind EqualsToken) ]

            //skipChar '!' >>. (skipChar '=' >>% TokenKind BangEqualsToken <|> preturn (TokenKind BangToken))
            //skipChar '|' >>. choice [ skipChar '>' >>% TokenKind PipeToken; skipString "||" >>% TokenKind BitwiseOrToken ]
            //skipChar '<' >>. choice [ skipChar '=' >>% TokenKind LessThanOrEqualToken; skipString "<<" >>% TokenKind BitshiftLeftToken; preturn (TokenKind LessThanToken) ]
            //skipChar '>' >>. choice [ skipChar '=' >>% TokenKind GreaterThanOrEqualToken; skipString ">>" >>% TokenKind BitshiftRightToken; preturn (TokenKind GreaterThanToken) ]
            //skipChar '{' >>% TokenKind OpenBraceToken
            //skipChar '}' >>% TokenKind CloseBraceToken
            //skipChar '[' >>% TokenKind OpenBracketToken
            //skipChar ']' >>% TokenKind CloseBracketToken

            skipChar ':' >>% TokenKind ColonToken
            //skipChar ':' >>. (skipString ":::" >>% TokenKind UnsafeTypeCastToken <|> preturn (TokenKind ColonToken))

            //skipChar ';' >>% TokenKind SemicolonToken
            //skipString "&&&" >>% TokenKind BitwiseAndToken
            //skipString "^^^" >>% TokenKind BitwiseXorToken
            //skipString "~~~" >>% TokenKind BitwiseNotToken
            //followedByString "\"" >>. Parse.stringLiteral '"' true |>> StringLiteralTokenData // TODO unterminated string
            //followedByString "#" >>. (inlineCppToken |>> InlineCppTokenData)
            
            
            followedBy digit >>. pint64 |>> IntLiteralTokenData
            
            followedBy (asciiLetter <|> pchar '_') >>. Parse.id |>> getKeywordOrIdentifierTokenData
            // TODO character array literals
            badToken
          ]

    let tokenArgsFromData = function
        | BadTokenData c -> BadToken, string c, None
        | IntLiteralTokenData i -> IntLiteralToken, string i, Some (IntValue i)
        | IdentifierTokenData identifierText -> IdentifierToken, identifierText, None
        | TokenKind k ->
            match k with
            | EndOfFileToken -> k, char 0 |> string, None
            | PlusToken -> k, "+", None
            | MinusToken -> k, "-", None
            | StarToken -> k, "*", None
            | SlashToken -> k, "/", None
            | OpenParenthesisToken -> k, "(", None
            | CloseParenthesisToken -> k, ")", None
            | CommaToken -> k, ",", None
            | EqualsToken -> k, "=", None
            | ColonToken -> k, ":", None
            | KeywordToken keyword -> k, (SyntaxFacts.keywordText keyword), None
            | BadToken
            | IdentifierToken
            | IntLiteralToken -> failwith "already handled"
            (*| PipeToken
            | BitwiseOrToken
            | BitwiseXorToken
            | BitwiseAndToken
            | BitwiseNotToken
            | EqualsEqualsToken
            | EqualsToken
            | BangEqualsToken
            | LessThanToken
            | LessThanOrEqualToken
            | GreaterThanToken
            | GreaterThanOrEqualToken
            | BangToken
            | BitshiftRightToken
            | BitshiftLeftToken
            | OpenBraceToken
            | CloseBraceToken
            | OpenBracketToken
            | CloseBracketToken
            | ColonToken
            | SemicolonToken
            | UnsafeTypeCastToken
            | CommaToken
            | ApostropheToken
            | ArrowToken
            | DoubleArrowToken -> k, "?", None
            *)
    let token : Parser<Token> =
        skipMany (pchar ' ') >>.
        pipe3
            getPosition
            tokenData
            getPosition
            (fun startPos tokenData endPos ->
                let (kind, text, value) = tokenData |> tokenArgsFromData
                {
                    start = startPos
                    text = Some text
                    end_ = endPos
                    value = value
                    tokenKind = kind
                }
            )

    let initP : Parser<CharStream<UserState>> = fun stream -> Reply(stream)

   // TODO lexer modes -> separate lexer for expr and types -> solves the issue of type arg vs character array literal; solves issue of pattern _ vs identifier _ ..?


type Lexer(streamName: string, text: string) =

    let (Success (stream, _, _)) = runParserOnString Lexer.initP (UserState.Create()) streamName text

    member __.Lex() : Token =
        // TODO return EofToken if the stream is at the end?
        let reply = Lexer.token stream
        if reply.Status = Ok then
            reply.Result
        else failwith "lexing a token should never fail"

    member __.GetErrors() =
        stream.UserState.parserErrors |> List.rev |> List.map (fun (a,_,_) -> a)

    member __.Position() =
        stream.Position