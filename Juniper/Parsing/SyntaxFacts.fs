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
    | MinusToken -> 12
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
    | _ -> None

let keywordText (keyword: Keyword) =
    match keyword with
    | ModuleKeyword -> "module"
    | OpenKeyword -> "open"
    | LetKeyword -> "let"
    | AliasKeyword -> "alias"
    | TypeKeyword -> "type"
    | FunKeyword -> "fun"