namespace Juniper.LanguageServer

open Ast
open OmniSharp.Extensions.LanguageServer.Protocol.Models

type Token =
    | KeywordToken of string
    | VariableToken of string
    | FunctionNameToken of string
    | TypeParameterToken of string
    | TypeToken
    | InlineCodeToken of string
    | ModuleNameToken of string
    | ModuleMemberToken of string
    | ValueConstructorToken of string
    | IncludeToken of string
    | BinaryOpToken of BinaryOps
    | UnaryOpToken of UnaryOps
    | UnionTypeNameToken of string
    | RecordFieldNameToken of string
    | UnitToken
    | IntegerToken of int64
    | FloatToken of float
    | TextToken of string

module Tokenizer =

    let tokenizeOption (tokenizer) opt: seq<Range * Token> = 
        match opt with
        | Some t -> tokenizer t
        | None -> Seq.empty

    let mapAdorn f ((startPos: FParsec.Position, endPos: FParsec.Position), data) =
        let startLine = startPos.Line |> int
        let startChar = startPos.Column |> int
        let endLine = endPos.Line |> int
        let endChar = endPos.Column |> int
        Range(startLine - 1, startChar - 1, endLine - 1, endChar - 1), f data

    let rec tokenizeAst (Module declarations): seq<Range * Token> =
        declarations |> Seq.collect (snd >> tokenizeDeclaration)
    and tokenizeDeclaration (declaration: Declaration) =
        match declaration with
        | FunctionDec functionDec ->
            seq {
                yield functionDec.name |> mapAdorn FunctionNameToken
                yield! functionDec.template |> Option.map snd |> tokenizeOption tokenizeTemplate
                yield! functionDec.clause |> snd |> tokenizeFunctionClause
            }
        | AliasDec aliasDec ->
            seq {
                yield aliasDec.name |> mapAdorn (fun _ -> TypeToken)
                yield! aliasDec.template |> Option.map snd |> tokenizeOption tokenizeTemplate
                yield! aliasDec.typ |> tokenizeTyExpr
            }
        | ModuleNameDec moduleName ->
            seq {
                moduleName |> mapAdorn ModuleNameToken
            }
        | OpenDec openDec ->
            openDec |> snd |> Seq.map (mapAdorn ModuleNameToken)
        | LetDec letDec ->
            seq {
                yield letDec.varName |> mapAdorn VariableToken
                yield! letDec.typ |> tokenizeOption tokenizeTyExpr
                yield! letDec.right |> tokenizeExpr
            }
        | InlineCodeDec inlineCode ->
            seq {
                inlineCode |> mapAdorn InlineCodeToken
            }
        | UnionDec unionDec ->
            seq {
                yield unionDec.name |> mapAdorn UnionTypeNameToken
                yield! unionDec.template |> Option.map snd |> tokenizeOption tokenizeTemplate 
                yield!
                    unionDec.valCons
                    |> snd
                    |> Seq.collect tokenizeValCon
            }
        | IncludeDec (_, includeDec) ->
            includeDec |> Seq.map (mapAdorn IncludeToken)
    and tokenizeValCon ((name, types): ValueCon) =
        seq {
            yield name |> mapAdorn ValueConstructorToken
            yield! types |> Seq.collect tokenizeTyExpr
        }
    and tokenizeTemplate (template: Template) =
        seq {
            yield!
                template.tyVars
                |> snd
                |> Seq.map (mapAdorn TypeParameterToken)
            yield!
                template.capVars
                |> Option.map (snd >> Seq.map (mapAdorn TypeParameterToken))
                |> Option.defaultValue (Seq.empty)
        }
    and tokenizeFunctionClause (clause: FunctionClause) =
        seq {
            yield! clause.arguments |> snd |> tokenizeArguments
            yield! clause.returnTy |> tokenizeOption (tokenizeTyExpr)
            yield! clause.body |> tokenizeExpr
        }
    and tokenizeTyExpr (ty: PosAdorn<TyExpr>) =
        seq {
            yield ty |> mapAdorn (fun _ -> TypeToken)
        }
    and tokenizeArguments (arguments: (PosAdorn<string> * PosAdorn<TyExpr> option) list) =
        arguments
        |> Seq.collect (fun (arg,tyOpt) ->
            seq {
                yield arg |> mapAdorn VariableToken
                yield! tyOpt |> Option.map (tokenizeTyExpr) |> Option.defaultValue Seq.empty
            }
        )
    and tokenizeExpr ((_, expr: Expr) as exprAdorn): seq<Range * Token> = 
        match expr with
        | SequenceExp (_, items) ->
            items
            |> Seq.collect tokenizeExpr
        | BinaryOpExp binaryOp ->
            seq {
                yield! binaryOp.left |> tokenizeExpr
                yield binaryOp.op |> mapAdorn BinaryOpToken
                yield! binaryOp.right |> tokenizeExpr
            }
        | IfElseExp ifElse ->
            seq {
                yield! ifElse.condition |> tokenizeExpr
                yield! ifElse.trueBranch |> tokenizeExpr
                yield! ifElse.falseBranch |> tokenizeExpr
            }
        | LetExp letExp ->
            seq {
                yield! letExp.left |> tokenizePattern
                yield! letExp.right |> tokenizeExpr
            }
        | InlineCode c -> [ c |> mapAdorn InlineCodeToken ]
        | AssignExp assignExp ->
            seq {
                yield assignExp.ref |> mapAdorn (fun _ -> KeywordToken "ref")
                yield! assignExp.left |> tokenizeLeftAssign
                yield! assignExp.right |> tokenizeExpr
            }
        | ForLoopExp forLoopExp ->
            seq {
                yield forLoopExp.varName |> mapAdorn VariableToken
                yield! forLoopExp.typ |> tokenizeOption (tokenizeTyExpr)
                yield! forLoopExp.start |> tokenizeExpr
                // TODO check the order
                yield! forLoopExp.direction |> tokenizeDirection
                yield! forLoopExp.end_ |> tokenizeExpr
            }
        | WhileLoopExp whileLoopExp ->
            seq {
                yield! whileLoopExp.condition |> tokenizeExpr
                yield! whileLoopExp.body |> tokenizeExpr
            }
        | DoWhileLoopExp doWhileLoopExp ->
            seq {
                yield! doWhileLoopExp.condition |> tokenizeExpr
                yield! doWhileLoopExp.body |> tokenizeExpr
            }
        | CaseExp caseRec ->
            seq {
                yield! caseRec.on |> tokenizeExpr
                yield!
                    caseRec.clauses
                    |> snd
                    |> Seq.collect tokenizeCaseClause
            }
        | UnaryOpExp unaryOpExp ->
            seq {
                yield unaryOpExp.op |> mapAdorn UnaryOpToken
                yield! unaryOpExp.exp |> tokenizeExpr
            }
        | RecordAccessExp recordAccess ->
            seq {
                yield! recordAccess.record |> tokenizeExpr
                yield recordAccess.fieldName |> mapAdorn RecordFieldNameToken
            }
        | ArrayAccessExp arrayAccessExp ->
            seq {
                yield! arrayAccessExp.array |> tokenizeExpr
                yield! arrayAccessExp.index |> tokenizeExpr
            }
        | VarExp varExp ->
            seq {
                yield varExp |> mapAdorn VariableToken
            }
        | DeclVarExp declVarExp ->
            seq {
                yield declVarExp.varName |> mapAdorn VariableToken
                yield! declVarExp.typ |> tokenizeTyExpr
            }
        | UnsafeTypeCast unsafeTypeCast ->
            seq {
                yield! unsafeTypeCast.typ |> tokenizeTyExpr
                yield! unsafeTypeCast.exp |> tokenizeExpr
            }
        | UnitExp unitExp ->
            seq {
                yield unitExp |> mapAdorn (fun _ -> UnitToken)
            }
        | TrueExp trueExp ->
            seq {
                yield trueExp |> mapAdorn (fun () -> KeywordToken "true")
            }
        | FalseExp falseExp ->
            seq {
                yield falseExp |> mapAdorn (fun () -> KeywordToken "false")
            }
        | LambdaExp (_, functionClause) ->
            functionClause |> tokenizeFunctionClause
        | IntExp i
        | Int8Exp i
        | UInt8Exp i
        | Int16Exp i
        | UInt16Exp i
        | Int32Exp i
        | UInt32Exp i
        | Int64Exp i
        | UInt64Exp i ->
            seq {
                yield i |> mapAdorn IntegerToken
            }
        | FloatExp f
        | DoubleExp f -> 
            seq {
                yield f |> mapAdorn FloatToken
            }
        | CharListLiteral text
        | StringLiteral text ->
            seq {
                yield text |> mapAdorn TextToken
            }
        | CallExp callExp ->
            seq {
                yield! callExp.func |> tokenizeExpr
                yield!
                    callExp.args
                    |> snd
                    |> Seq.collect (tokenizeExpr)
            }
        | TemplateApplyExp templateApply -> []
        | ModQualifierExp (_, modQualifier) ->
            seq {
                yield modQualifier.module_ |> mapAdorn ModuleNameToken
                yield modQualifier.name |> mapAdorn ModuleMemberToken
            }
        | RecordExp recordExp ->
            seq {
                yield!
                    recordExp.packed
                    |> Option.toList
                    |> Seq.map (mapAdorn (fun () ->KeywordToken "packed"))
                yield!
                    recordExp.initFields
                    |> snd
                    |> Seq.collect (fun (recordField, initValue) ->
                        seq {
                            yield recordField |> mapAdorn RecordFieldNameToken
                            yield! initValue |> tokenizeExpr
                        }
                    )
            }
        | ArrayLitExp _ -> []
        | ArrayMakeExp _ -> []
        | RefExp _ -> []
        | TupleExp _ -> []
        | QuitExp _ -> []
        | TypeConstraint _ -> []
        | Smartpointer _ -> []
        | NullExp _ -> []
    and tokenizePattern pattern = []
    and tokenizeLeftAssign leftAssign = []
    and tokenizeDirection direction = []
    and tokenizeCaseClause caseClause = []
