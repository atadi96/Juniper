module Parsing.SyntaxFacts

open Ast
open Tokens

let getUnaryOperatorPrecedence (unaryOp: UnaryOps) =
    match unaryOp with
    | LogicalNot -> 12
    | BitwiseNot -> 12
    | Negate -> 12
    | Deref -> 12

let getBinaryOperatorPrecedence (binaryOp: BinaryOps) =
    match binaryOp with
    | Add | Subtract | Multiply | Divide | Modulo | BitwiseOr | BitwiseAnd | BitwiseXor
    | LogicalOr | LogicalAnd | Equal | NotEqual | GreaterOrEqual | LessOrEqual | Greater | Less
    | BitshiftLeft | BitshiftRight | Pipe -> 0

let getKeyword (text: string) =
    match text with
    | "module" -> Some ModuleKeyword
    | "open" -> Some OpenKeyword
    | "let" -> Some LetKeyword
    | _ -> None

let keywordText (keyword: Keyword) =
    match keyword with
    | ModuleKeyword -> "module"
    | OpenKeyword -> "open"
    | LetKeyword -> "let"
