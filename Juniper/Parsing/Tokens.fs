module Parsing.Tokens

type Position =
    {
        streamName: string
        lineIndex: int
        colIndex: int
        index: int
    }

module Position =
    let fromFParsec (fparsecPosition: FParsec.Position) =
        {
            streamName = fparsecPosition.StreamName
            lineIndex = int fparsecPosition.Line - 1
            colIndex = int fparsecPosition.Column - 1
            index = int fparsecPosition.Index
        }

    let toFParsec (pos: Position) =
        FParsec.Position(pos.streamName, pos.index, int64 pos.lineIndex + 1L, int64 pos.colIndex + 1L)

type IntSuffix =
    | Int8Suffix
    | Int16Suffix
    | Int32Suffix
    | Int64Suffix
    | UInt8Suffix
    | UInt16Suffix
    | UInt32Suffix
    | UInt64Suffix

type TokenValue =
    | TextValue of string
    | IntValue of int64 * IntSuffix option
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
    | AndKeyword
    | OrKeyword
    | ModKeyword
    | IfKeyword
    | ThenKeyword
    | ElifKeyword
    | ElseKeyword
    | VarKeyword
    | ArrayKeyword
    | SetKeyword
    | ForKeyword
    | InKeyword
    | ToKeyword
    | DownToKeyword
    | DoKeyword
    | WhileKeyword

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
    | PipeOperatorToken
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
    | InlineCppToken
    | ArrowToken
    | DoubleArrowToken
    | StringLiteralToken
    | CharacterArrayLiteralToken
    | CommaToken
    | IntLiteralToken
    | IdentifierToken
    | TypeVariableIdentifierToken
    | DotToken

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
        startPos: Position
        endPos: Position
    }

type Token =
    {
        text: string option
        value: TokenValue option
        start: Position
        end_: Position
        tokenKind: TokenKind
        leadingTrivia: SyntaxTrivia list
        trailingTrivia: SyntaxTrivia list
    }
