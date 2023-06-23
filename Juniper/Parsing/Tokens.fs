module Parsing.Tokens

type TokenValue =
    | TextValue of string
    | IntValue of int64
    | BaseTypeValue of Ast.BaseTypes
    | TypeVariableIdentifierValue of string
    | InlineCppCodeValue of string
    | BoolValue of bool

type Keyword =
    | ModuleKeyword
    | OpenKeyword
    | LetKeyword
    | TypeKeyword
    | AliasKeyword
    | FunKeyword
    | UInt8Keyword
    | UInt16Keyword
    | UInt32Keyword
    | UInt64Keyword
    | Int8Keyword
    | Int16Keyword
    | Int32Keyword
    | Int64Keyword
    | BoolKeyword
    | UnitKeyword
    | FloatKeyword
    | DoubleKeyword
    | PointerKeyword
    | StringKeyword
    | RawPointerKeyword
    | PackedKeyword
    | NotKeyword
    | FnKeyword
    | EndKeyword
    | RefKeyword
    | TrueKeyword
    | FalseKeyword
    | NullKeyword
    | CaseKeyword
    | OfKeyword
    | MutableKeyword

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
    | BitwiseNotToken
    | EqualsEqualsToken
    | EqualsToken
    //| BangEqualsToken
    | LessThanToken
    //| LessThanOrEqualToken
    | GreaterThanToken
    //| GreaterThanOrEqualToken
    | BangToken
    //| BitshiftRightToken
    //| BitshiftLeftToken
    | OpenBraceToken
    | CloseBraceToken
    | OpenBracketToken
    | CloseBracketToken
    | ColonToken
    | SemicolonToken
    //| UnsafeTypeCastToken
    //| ApostropheToken
    | InlineCppToken
    | ArrowToken
    | DoubleArrowToken
    | StringLiteralToken
    | CharacterArrayLiteralToken
    | CommaToken
    | IntLiteralToken
    | IdentifierToken
    | TypeVariableIdentifierToken



type SyntaxTriviaKind =
    | SingleLineCommentTrivia
    | MultiLineCommentTrivia
    | WhiteSpaceTrivia
    | BadTokenTrivia
    | LineBreakTrivia

type SyntaxTrivia =
    {
        triviaKind: SyntaxTriviaKind
        text: string
        startPos: FParsec.Position
        endPos: FParsec.Position
    }

type Token =
    {
        text: string option
        value: TokenValue option
        start: FParsec.Position
        end_: FParsec.Position
        tokenKind: TokenKind
        leadingTrivia: SyntaxTrivia list
        trailingTrivia: SyntaxTrivia list
    }