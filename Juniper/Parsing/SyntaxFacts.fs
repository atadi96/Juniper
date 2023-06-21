module Parsing.SyntaxFacts

open Ast
open Tokens
(*
let getUnaryOperatorPrecedence (unaryOp: UnaryOps) =
    match unaryOp with
    | LogicalNot -> 12
    | BitwiseNot -> 12
    | Negate -> 12
    | Deref -> 12
*)
let getUnaryOperatorPrecedence (tokenKind: TokenKind) =
    match tokenKind with
    | MinusToken
    | KeywordToken RefKeyword
    | BitwiseNotToken
    | BangToken -> 12
    | _ -> 0
(*
let getBinaryOperatorPrecedence (binaryOp: BinaryOps) =
    match binaryOp with
    | Add | Subtract | Multiply | Divide | Modulo | BitwiseOr | BitwiseAnd | BitwiseXor
    | LogicalOr | LogicalAnd | Equal | NotEqual | GreaterOrEqual | LessOrEqual | Greater | Less
    | BitshiftLeft | BitshiftRight | Pipe -> 0
*)

let getBinaryOperatorPrecedence (tokenKind: TokenKind) =
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
