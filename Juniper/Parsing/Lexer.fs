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
   
and FParser<'t> = Parser<'t, UserState>

type LexerMode =
    | ExpressionMode
    | TypeMode

module private Lexer =
    type TokenData =
         | TokenKind of TokenKind
         | IntLiteralTokenData of int64
         | BadTokenData of char
         | BaseTypeKeywordData of Keyword * Ast.BaseTypes
         | InlineCppTokenData of string
         | StringLiteralTokenData of string
         | CharacterArrayLiteralTokenData of string
         | IdentifierTokenData of string
         | TypeVariableTokenData of string

    let badToken : FParser<TokenData> =
         fun stream ->
             stream.UserState <-
                 stream
                     .UserState
                     .PrependError(stream, sprintf "Unexpected character %c" (stream.Peek()))

             (anyChar |>> BadTokenData) stream
    
    let inlineCppToken : FParser<string> =
        let normalCharSnippet = manySatisfy (fun c -> c <> '\\' && c <> '#')
        let escapedChar = pstring "\\" >>. (anyOf "\\#" |>> string)
        between (pstring "#") (pstring "#") ((stringsSepBy normalCharSnippet escapedChar))

    let private reportUnterminated name ctor (p: FParser<_>): FParser<_> =
        fun stream ->
            let initialState = stream.State
            let reply = p stream
            if reply.Status = FatalError then
                stream.UserState <-
                    stream.UserState.PrependError(stream, sprintf "Unterminated %s" name)
                let text = stream.ReadFrom(initialState, false)
                Reply(ctor text)
            else
                Reply(reply.Status, ctor reply.Result, reply.Error)

    let private parseTrivia (leading: bool) : FParser<SyntaxTrivia list> =
        let singleLineComment =
            skipString "//" >>. skipRestOfLine false
            |> skipped
            |>> (fun singleLineComment -> SingleLineCommentTrivia, singleLineComment)

        let multiLineComment: FParser<_> =
            skipString "(*"
            >>. (skipCharsTillString "*)" false System.Int32.MaxValue .>> skipString "*)" |> Parse.fatalizeAnyError)
            |> skipped
            |> reportUnterminated "multi-line comment" id
            |>> (fun multiLineComment -> MultiLineCommentTrivia, multiLineComment)

        let whiteSpaces = many1Chars (anyOf ['\t'; ' ']) |>> fun ws -> WhiteSpaceTrivia, ws

        let lineBreak =
            skipNewline
            |> skipped
            |>> (fun lineBreakText -> LineBreakTrivia, lineBreakText)

        let trivia p =
            pipe3
                getPosition
                p
                getPosition
                (fun startPos (triviaKind, text) endPos ->
                    {
                        triviaKind = triviaKind
                        text = text
                        startPos = startPos
                        endPos = endPos
                    }
                )

        if leading then
            many (choice [singleLineComment; multiLineComment; whiteSpaces; lineBreak] |> trivia)
        else
            many (choice [singleLineComment; multiLineComment; whiteSpaces] |> trivia)
            .>>. opt (lineBreak |> trivia)
            |>> fun (trivias, optionalLineBreak) ->
                match optionalLineBreak with
                | None -> trivias
                | Some lineBreakFound ->
                    trivias @ [lineBreakFound]

    let parseLeadingTrivia = parseTrivia true

    let parseTrailingTrivia = parseTrivia false

    let private getKeywordOrIdentifierTokenData (text: string) =
        text
        |> SyntaxFacts.getKeyword
        |> Option.map (fun keyword ->
            keyword
            |> SyntaxFacts.getKeywordBaseType
            |> Option.map (fun baseType -> BaseTypeKeywordData (keyword, baseType))
            |> Option.defaultValue (TokenKind (KeywordToken keyword))
        )
        |> Option.defaultValue (IdentifierTokenData text)

    let tokenData mode : FParser<TokenData> =
        choice
          [
            eof >>% TokenKind EndOfFileToken

            skipChar '+' >>% TokenKind PlusToken
            skipChar '-' >>. (skipChar '>' >>% TokenKind ArrowToken <|> preturn (TokenKind MinusToken))
            
            skipChar '*' >>% TokenKind StarToken
            skipChar '/' >>% TokenKind SlashToken

            skipChar '(' >>% TokenKind OpenParenthesisToken
            skipChar ')' >>% TokenKind CloseParenthesisToken

            skipChar ',' >>% TokenKind CommaToken
            
            skipChar '=' >>. choice [ skipChar '=' >>% TokenKind EqualsEqualsToken; skipChar '>' >>% TokenKind DoubleArrowToken; preturn (TokenKind EqualsToken) ]

            skipChar '!' >>% TokenKind BangToken
            //skipChar '!' >>. (skipChar '=' >>% TokenKind BangEqualsToken <|> preturn (TokenKind BangToken))

            skipChar '|' >>% TokenKind PipeToken
            //skipChar '|' >>. choice [ skipChar '>' >>% TokenKind PipeOperatorToken; skipString "||" >>% TokenKind BitwiseOrToken ]

            skipChar '<' >>% TokenKind LessThanToken
            //skipChar '<' >>. choice [ skipChar '=' >>% TokenKind LessThanOrEqualToken; skipString "<<" >>% TokenKind BitshiftLeftToken; preturn (TokenKind LessThanToken) ]

            skipChar '>' >>% TokenKind GreaterThanToken
            //skipChar '>' >>. choice [ skipChar '=' >>% TokenKind GreaterThanOrEqualToken; skipString ">>" >>% TokenKind BitshiftRightToken; preturn (TokenKind GreaterThanToken) ]
            skipChar '{' >>% TokenKind OpenBraceToken
            skipChar '}' >>% TokenKind CloseBraceToken
            skipChar '[' >>% TokenKind OpenBracketToken
            skipChar ']' >>% TokenKind CloseBracketToken

            skipChar ':' >>% TokenKind ColonToken
            //skipChar ':' >>. (skipString ":::" >>% TokenKind UnsafeTypeCastToken <|> preturn (TokenKind ColonToken))

            skipChar ';' >>% TokenKind SemicolonToken
            //skipString "&&&" >>% TokenKind BitwiseAndToken
            //skipString "^^^" >>% TokenKind BitwiseXorToken
            skipString "~~~" >>% TokenKind BitwiseNotToken

            followedByString "\"" >>. (Parse.stringLiteral '"' true |> Parse.fatalizeAnyError) |> reportUnterminated "string literal" StringLiteralTokenData

            if mode = ExpressionMode then
                followedByString "\'" >>. (Parse.stringLiteral '\'' true |> Parse.fatalizeAnyError) |> reportUnterminated "character array literal" CharacterArrayLiteralTokenData
            else
                skipChar '\'' >>. Parse.id |>> TypeVariableTokenData
            
            followedByString "#" >>. (inlineCppToken |> Parse.fatalizeAnyError) |> reportUnterminated "inline C++ code" InlineCppTokenData
            
            
            followedBy digit >>. pint64 |>> IntLiteralTokenData
            
            followedBy (asciiLetter <|> pchar '_') >>. Parse.id |>> getKeywordOrIdentifierTokenData

            badToken
          ]

    let tokenArgsFromData = function
        | BadTokenData c -> BadToken, string c, None
        | IntLiteralTokenData i -> IntLiteralToken, string i, Some (IntValue i)
        | IdentifierTokenData identifierText -> IdentifierToken, identifierText, None
        | BaseTypeKeywordData (keyword, baseType) ->
            KeywordToken keyword, (SyntaxFacts.keywordText keyword), Some (BaseTypeValue baseType)
        | TypeVariableTokenData typeVariableName ->
            TypeVariableIdentifierToken, "'" + typeVariableName, Some (TypeVariableIdentifierValue typeVariableName)
        | CharacterArrayLiteralTokenData literal ->
            CharacterArrayLiteralToken, "'" + literal + "'", Some (TextValue literal)
        | StringLiteralTokenData literal ->
            StringLiteralToken, "\"" + literal + "\"", Some (TextValue literal)
        | InlineCppTokenData cppCode ->
            InlineCppToken, "#" + cppCode + "#", Some (InlineCppCodeValue cppCode)
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
            | PipeToken -> k, "|", None
            //| BitwiseOrToken
            //| BitwiseXorToken
            //| BitwiseAndToken
            | BitwiseNotToken -> k, "~~~", None
            | EqualsEqualsToken -> k, "==", None
            //| BangEqualsToken
            | LessThanToken -> k, "<", None
            //| LessThanOrEqualToken
            | GreaterThanToken -> k, ">", None
            //| GreaterThanOrEqualToken
            | BangToken -> k, "!", None
            //| BitshiftRightToken
            //| BitshiftLeftToken
            | OpenBraceToken -> k, "{", None
            | CloseBraceToken -> k, "}", None
            | OpenBracketToken -> k, "[", None
            | CloseBracketToken -> k, "]", None
            | SemicolonToken -> k, ";", None
            //| UnsafeTypeCastToken
            //| ApostropheToken
            | ArrowToken -> k, "->", None
            | DoubleArrowToken -> k, "=>", None
            | BadToken
            | IdentifierToken
            | TypeVariableIdentifierToken
            | CharacterArrayLiteralToken
            | StringLiteralToken
            | InlineCppToken
            | IntLiteralToken -> failwith "already handled"
    let token mode : FParser<Token> =
        pipe5
            parseLeadingTrivia
            getPosition
            (tokenData mode)
            getPosition
            parseTrailingTrivia
            (fun leadingTrivia startPos tokenData endPos trailingTrivia ->
                let (kind, text, value) = tokenData |> tokenArgsFromData
                {
                    start = startPos
                    text = Some text
                    end_ = endPos
                    value = value
                    tokenKind = kind
                    leadingTrivia = leadingTrivia
                    trailingTrivia = trailingTrivia
                }
            )

    let initP : FParser<CharStream<UserState>> = fun stream -> Reply(stream)

type Lexer(streamName: string, text: string) =

    let mutable mode = ExpressionMode

    let token =
        let expressionToken = Lexer.token ExpressionMode
        let typeToken = Lexer.token TypeMode
        fun mode stream ->
            match mode with
            | ExpressionMode -> expressionToken stream
            | TypeMode -> typeToken stream

    let (Success (stream, _, _)) = runParserOnString Lexer.initP (UserState.Create()) streamName text

    member __.Lex() : Token =
        // TODO return EofToken if the stream is at the end?
        let reply = token mode stream
        if reply.Status = Ok then
            reply.Result
        else failwithf "lexing a token should never fail - '%c' %A" (stream.Peek()) (stream.Position)

    member __.GetErrors() =
        stream.UserState.parserErrors |> List.rev |> List.map (fun (a,_,_) -> a)

    member __.Position() =
        stream.Position

    member __.SetMode(lexerMode) =
        mode <- lexerMode

    member __.CurrentMode() = mode

    member this.Peek1() : Token =
        let currentState = stream.State
        let token = this.Lex()
        stream.BacktrackTo currentState
        token