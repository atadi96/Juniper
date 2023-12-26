module Parsing.SyntaxFacts

open Ast
open Tokens

let getUnaryOperatorPrecedence (tokenKind: TokenKind) =
    match tokenKind with
    | MinusToken
    | KeywordToken NotKeyword
    | BitwiseNotToken
    | BangToken -> 12
    | _ -> 0

let getBinaryOperatorPrecedence (tokenKind: TokenKind) =
    match tokenKind with
    | PipeOperatorToken -> 1
    | KeywordToken OrKeyword -> 2
    | KeywordToken AndKeyword -> 3
    | BitwiseOrToken -> 4
    | BitwiseAndToken -> 6
    | EqualsEqualsToken
    | BangEqualsToken -> 7
    | LessThanToken
    | GreaterThanToken
    | LessThanOrEqualToken
    | GreaterThanOrEqualToken -> 8
    | BitshiftRightToken
    | BitshiftLeftToken -> 9
    | PlusToken
    | MinusToken -> 10
    | StarToken
    | SlashToken
    | KeywordToken ModKeyword -> 11
    | _ -> 0

let getUnaryCapacityOperatorPrecedence (tokenKind: TokenKind) =
    match tokenKind with
    // TODO: negate
    | _ -> 0

let getBinaryCapacityOperatorPrecedence (tokenKind: TokenKind) =
    match tokenKind with
    | PlusToken
    | MinusToken -> 10
    | StarToken
    | SlashToken -> 11
    | _ -> 0

let getKeyword (text: string) =
    match text with
    | "module" -> Some ModuleKeyword
    | "open" -> Some OpenKeyword
    | "let" -> Some LetKeyword
    | "alias" -> Some AliasKeyword
    | "type" -> Some TypeKeyword
    | "fun" -> Some FunKeyword
    | "uint8" -> Some UInt8Keyword
    | "uint16" -> Some UInt16Keyword
    | "uint32" -> Some UInt32Keyword
    | "uint64" -> Some UInt64Keyword
    | "int8" -> Some Int8Keyword
    | "int16" -> Some Int16Keyword
    | "int32" -> Some Int32Keyword
    | "int64" -> Some Int64Keyword
    | "bool" -> Some BoolKeyword
    | "unit" -> Some UnitKeyword
    | "float" -> Some FloatKeyword
    | "double" -> Some DoubleKeyword
    | "pointer" -> Some PointerKeyword
    | "string" -> Some StringKeyword
    | "rawpointer" -> Some RawPointerKeyword
    | "packed" -> Some PackedKeyword
    | "not" -> Some NotKeyword
    | "fn" -> Some FnKeyword
    | "end" -> Some EndKeyword
    | "ref" -> Some RefKeyword
    | "true" -> Some TrueKeyword
    | "false" -> Some FalseKeyword
    | "null" -> Some NullKeyword
    | "case" -> Some CaseKeyword
    | "of" -> Some OfKeyword
    | "mutable" -> Some MutableKeyword
    | "and" -> Some AndKeyword
    | "or" -> Some OrKeyword
    | "mod" -> Some ModKeyword
    | "if" -> Some IfKeyword
    | "elif" -> Some ElifKeyword
    | "then" -> Some ThenKeyword
    | "else" -> Some ElseKeyword
    | "var" -> Some VarKeyword
    | "set" -> Some SetKeyword
    | "for" -> Some ForKeyword
    | "in" -> Some InKeyword
    | "to" -> Some ToKeyword
    | "downto" -> Some DownToKeyword
    | "do" -> Some DoKeyword
    | "while" -> Some WhileKeyword
    | "array" -> Some ArrayKeyword
    | "smartpointer" -> Some SmartPointerKeyword
    | "where" -> Some WhereKeyword
    | "num" -> Some NumKeyword
    | "int" -> Some IntKeyword
    | "real" -> Some RealKeyword
    | _ -> None

let keywordText (keyword: Keyword) =
    match keyword with
    | ModuleKeyword -> "module"
    | OpenKeyword -> "open"
    | LetKeyword -> "let"
    | AliasKeyword -> "alias"
    | TypeKeyword -> "type"
    | FunKeyword -> "fun" 
    | UInt8Keyword -> "uint8"
    | UInt16Keyword -> "uint16"
    | UInt32Keyword -> "uint32"
    | UInt64Keyword -> "uint64"
    | Int8Keyword -> "int8"
    | Int16Keyword -> "int16"
    | Int32Keyword -> "int32"
    | Int64Keyword -> "int64"
    | BoolKeyword -> "bool"
    | UnitKeyword -> "unit"
    | FloatKeyword -> "float"
    | DoubleKeyword -> "double"
    | PointerKeyword -> "pointer"
    | StringKeyword -> "string"
    | RawPointerKeyword -> "rawpointer"
    | PackedKeyword -> "packed"
    | NotKeyword -> "not"
    | FnKeyword -> "fn"
    | EndKeyword -> "end"
    | RefKeyword -> "ref"
    | TrueKeyword -> "true"
    | FalseKeyword -> "false"
    | NullKeyword -> "null"
    | CaseKeyword -> "case"
    | OfKeyword -> "of"
    | MutableKeyword -> "mutable"
    | AndKeyword -> "and"
    | OrKeyword -> "or"
    | ModKeyword -> "mod"
    | IfKeyword -> "if"
    | ElifKeyword -> "elif"
    | ThenKeyword -> "then"
    | ElseKeyword -> "else"
    | VarKeyword -> "var"
    | SetKeyword -> "set"
    | ForKeyword -> "for"
    | InKeyword -> "in"
    | ToKeyword -> "to"
    | DownToKeyword -> "downto"
    | DoKeyword -> "do"
    | WhileKeyword -> "while"
    | ArrayKeyword -> "array"
    | SmartPointerKeyword -> "smartpointer"
    | WhereKeyword -> "where"
    | NumKeyword -> "num"
    | IntKeyword -> "int"
    | RealKeyword -> "real"

let getKeywordBaseType (keyword: Keyword) =
    match keyword with
    | UInt8Keyword -> Some Ast.BaseTypes.TyUint8
    | UInt16Keyword -> Some Ast.BaseTypes.TyUint16
    | UInt32Keyword -> Some Ast.BaseTypes.TyUint32
    | UInt64Keyword -> Some Ast.BaseTypes.TyUint64
    | Int8Keyword -> Some Ast.BaseTypes.TyInt8
    | Int16Keyword -> Some Ast.BaseTypes.TyInt16
    | Int32Keyword -> Some Ast.BaseTypes.TyInt32
    | Int64Keyword -> Some Ast.BaseTypes.TyInt64
    | BoolKeyword -> Some Ast.BaseTypes.TyBool
    | UnitKeyword -> Some Ast.BaseTypes.TyUnit
    | FloatKeyword -> Some Ast.BaseTypes.TyFloat
    | DoubleKeyword -> Some Ast.BaseTypes.TyDouble
    | PointerKeyword -> Some Ast.BaseTypes.TyPointer
    | StringKeyword -> Some Ast.BaseTypes.TyString
    | RawPointerKeyword -> Some Ast.BaseTypes.TyRawPointer
    | _ -> None
