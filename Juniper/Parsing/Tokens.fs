module Parsing.Tokens

type TokenValue =
    //| StringValue of string
    | IntValue of int64

type Keyword =
    | ModuleKeyword
    | OpenKeyword
    | LetKeyword
    | TypeKeyword
    | AliasKeyword

type TokenKind =
    | BadToken
    | EndOfFileToken
    | PlusToken
    | MinusToken
    | StarToken
    | SlashToken
    | OpenParenthesisToken
    | CloseParenthesisToken
    | KeywordToken of Keyword
    | PipeToken
    //| BitwiseOrToken
    //| BitwiseXorToken
    //| BitwiseAndToken
    //| BitwiseNotToken
    //| EqualsEqualsToken
    | EqualsToken
    //| BangEqualsToken
    | LessThanToken
    //| LessThanOrEqualToken
    | GreaterThanToken
    //| GreaterThanOrEqualToken
    //| BangToken
    //| BitshiftRightToken
    //| BitshiftLeftToken
    //| OpenBraceToken
    //| CloseBraceToken
    //| OpenBracketToken
    //| CloseBracketToken
    | ColonToken
    //| SemicolonToken
    //| UnsafeTypeCastToken
    //| ApostropheToken
    //| InlineCppToken
    //| ArrowToken
    //| DoubleArrowToken
    //| StringLiteralToken
    | CommaToken
    | IntLiteralToken
    | IdentifierToken

type Token =
    {
        text: string option
        value: TokenValue option
        start: FParsec.Position
        end_: FParsec.Position
        tokenKind: TokenKind
        //leading trivia
        //ending trivia
    }