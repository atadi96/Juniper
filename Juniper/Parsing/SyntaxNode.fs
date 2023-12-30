module Parsing.SyntaxNode

open Tokens
open SyntaxTree

type NodeKind =
    | TokenNodeKind of TokenKind
    | ModuleDefinitionKind
    | ModuleNameKind
    | OpenModulesKind
    | AlgebraicTypeKind
    | LetDeclarationKind
    | AliasKind
    | InlineCppDeclarationKind
    | IncludeDeclarationKind
    | TemplateDeclarationKind
    | TemplateDeclarationCapacityIdentifiersKind
    | TemplateApplicationKind
    | TemplateApplicationCapacityExpressionsKind
    | NaturalNumberCapacityExpressionKind
    | BinaryCapacityExpressionKind
    | IdentifierCapacityExpressionKind
    | ValueConstructorKind
    | FunctionDeclarationKind
    | WhereConstraintsKind
    | TypeConstraintKind
    | NumConstraintKind
    | IntConstraintKind
    | RealConstraintKind
    | PackedConstraintKind
    | MemberConstraintKind
    | SingleMemberConstraintKind
    | ModuleQualifierKind
    | IdentifierDeclarationReferenceKind
    | FunctionTypeExpressionKind
    | ClosureTypeExpressionKind
    | IdentifierWithOptionalTypeKind
    | RecordTypeExpressionKind
    | TupleTypeExpressionKind
    | FunctionCallExpressionKind
    | IfExpressionKind
    | ElifBranchKind
    | LetExpressionKind
    | VariableDeclarationKind
    | SetExpressionKind
    | SetRefExpressionKind
    | ForLoopExpressionKind
    | DoWhileExpressionKind
    | WhileDoExpressionKind
    | LambdaExpressionKind
    | CaseOfExpressionKind
    | CaseClauseKind
    | RefExpressionKind
    | ArrayLiteralExpressionKind
    | UnsafeTypeCastKind
    | VariablePatternKind
    | ValConPatternKind
    | ClosureTypeVariableKind
    | DeclarationReferenceTypeExpressionKind
    | BuiltInTypeExpressionKind
    | RefTypeExpressionKind
    | CapacityTypeExpressionKind
    | TypeVariableIdentifierTypeExpressionKind
    | ParenthesizedTypeExpressionSyntaxKind
    | UnitLiteralExpressionKind
    | BoolLiteralExpressionKind
    | NullLiteralExpressionKind
    | UnaryExpressionKind
    | BinaryExpressionKind
    | NumberExpressionKind
    | ParenthesizedExpressionKind
    | SequenceExpressionKind
    | StringLiteralExpressionKind
    | CharacterArrayLiteralExpressionKind
    | DeclarationReferenceExpressionKind
    | ArrayExpressionKind
    | ArrayInitializerKind
    | IndexerExpressionKind
    | MemberAccessExpressionKind
    | TypedExpressionKind
    | InlineCppExpressionKind
    | SmartPointerExpressionKind
    | FieldAssignKind
    | RecordExpressionKind
    | TupleExpressionKind
    | ArrayAccessLeftAssignKind
    | RecordMemberLeftAssignKind
    | DeclarationReferenceLeftAssignKind
    | IntegerPatternKind
    | TruePatternKind
    | FalsePatternKind
    | UnderscorePatternKind
    | UnitPatternKind
    | FieldPatternKind
    | RecordPatternKind
    | TuplePatternKind
    | ParenthesizedPatternKind
    | SyntaxTreeKind

type ISyntaxNode =
    abstract Kind: NodeKind
    abstract FullStart: Position
    abstract Start: Position
    abstract End: Position
    abstract FullEnd: Position
    abstract Children: ISyntaxNode seq

type TokenSyntaxNode(token: Tokens.Token) =
    let fullStart =
        token.leadingTrivia
        |> List.tryHead
        |> Option.map (fun trivia -> trivia.startPos)
        |> Option.defaultValue token.start
    let fullEnd =
        token.trailingTrivia
        |> List.tryLast
        |> Option.map (fun trivia -> trivia.endPos)
        |> Option.defaultValue token.end_

    member __.Token = token
    interface ISyntaxNode with
        member __.Kind = TokenNodeKind token.tokenKind
        member __.FullStart = fullStart
        member __.Start = token.start
        member __.End = token.end_
        member __.FullEnd = fullEnd
        member __.Children = []

type ChildrenSyntaxNode(nodeKind: NodeKind, children: ISyntaxNode seq) =
    let children = children |> List.ofSeq
    let (first,last) = children |> List.head, children |> List.last
    interface ISyntaxNode with
        member __.Kind = nodeKind
        member __.FullStart = first.FullStart
        member __.Start = first.Start
        member __.End = last.End
        member __.FullEnd = last.FullEnd
        member __.Children = children

open SyntaxTree.Patterns
open SyntaxTree.Declarations
open SyntaxTree.Expressions
open SyntaxTree.Types
open SyntaxTree.Modules

type SyntaxNode =
    static member HighlightTo (writer: System.IO.TextWriter) (node: ISyntaxNode) =
        let print format = Printf.ksprintf (Printf.fprintf writer "%s") format
        let isToConsole = writer = System.Console.Out
        let setColor c =
            if isToConsole then
                System.Console.ForegroundColor <- c
        let resetColor() =
            if isToConsole then
                System.Console.ResetColor()
        let rec write hasError (node: ISyntaxNode) =
            let writeTrivia (trivia: SyntaxTrivia) =
                let color =
                    match trivia.triviaKind with
                    | LineBreakTrivia
                    | WhiteSpaceTrivia -> System.ConsoleColor.White
                    | SingleLineCommentTrivia
                    | MultiLineCommentTrivia -> System.ConsoleColor.DarkGreen
                    | BadTokenTrivia -> System.ConsoleColor.DarkRed
                setColor color
                print "%s" trivia.text
                resetColor()
            match node with
            | :? TokenSyntaxNode as token ->
                token.Token.leadingTrivia |> Seq.iter writeTrivia

                let tokenColor =
                    match token.Token.tokenKind with
                    | KeywordToken _ 
                    | ArrowToken -> System.ConsoleColor.Blue
                    | IdentifierToken -> System.ConsoleColor.Yellow
                    | IntLiteralToken
                    | StringLiteralToken -> System.ConsoleColor.DarkYellow
                    | TypeVariableIdentifierToken -> System.ConsoleColor.Green
                    | InlineCppToken -> System.ConsoleColor.DarkBlue
                    | _ -> System.ConsoleColor.White

                setColor (if hasError then System.ConsoleColor.DarkRed else tokenColor)

                token.Token.text |> Option.iter (print "%s")

                token.Token.trailingTrivia |> Seq.iter writeTrivia

                let hasError = token.Token.text |> Option.isNone

                hasError
            | _ ->
                node.Children |> Seq.fold write hasError
        write false node
        |> ignore
        resetColor()

    static member WriteTo (writer: System.IO.TextWriter) (node: ISyntaxNode) =
        let print format = Printf.ksprintf (Printf.fprintf writer "%s") format
        let isToConsole = writer = System.Console.Out
        let setColor c =
            if isToConsole then
                System.Console.ForegroundColor <- c
        let resetColor() =
            if isToConsole then
                System.Console.ResetColor()
        let getMarker (isLast, trailingItems) =
            match isLast, trailingItems with
            | true, [] -> "└──"
            | _ -> "├──"
        let printTrivia (indent: string) prefix isLast trivias =
            let rec inner (trivias: SyntaxTrivia list) =
                match trivias with
                | [] -> ()
                | x :: xs ->
                    let marker = (isLast, xs) |> getMarker
                    setColor System.ConsoleColor.DarkGray
                    print "%s" indent
                    print "%s" marker
                    if x.triviaKind = BadTokenTrivia then
                        setColor System.ConsoleColor.DarkRed
                        print "%s: %A %A <%i:%i>" prefix x.triviaKind x.text x.startPos.lineIndex x.startPos.colIndex
                    else
                        setColor System.ConsoleColor.DarkGreen
                        print "%s: %A" prefix x.triviaKind
                    print "%s" System.Environment.NewLine
                    inner xs
            inner trivias
        let rec writeChildren indent children =
            match children with
            | [] -> ()
            | [ last ] -> write indent true last
            | child :: rest ->
                write indent false child
                writeChildren indent rest
        and write (indent: string) isLast (node: ISyntaxNode) =
            match node with
            | :? TokenSyntaxNode as token ->

                printTrivia indent "L" false token.Token.leadingTrivia

                let tokenMarker = (isLast, token.Token.trailingTrivia) |> getMarker

                setColor System.ConsoleColor.DarkGray
                print "%s" indent
                print "%s" tokenMarker

                match token.Token.text with
                | None ->
                    setColor System.ConsoleColor.DarkRed
                    print "%A <%i:%i>" token.Token.tokenKind token.Token.start.lineIndex token.Token.start.colIndex
                | Some tokenText ->
                    setColor System.ConsoleColor.Blue
                    print "%A" token.Token.tokenKind
                    token.Token.value |> Option.iter (print " %A")
                    setColor (
                        match token.Token.tokenKind with
                        | KeywordToken _ -> System.ConsoleColor.DarkYellow
                        | _ -> System.ConsoleColor.White
                    )
                    print " %s" tokenText

                resetColor()
                print "%s" System.Environment.NewLine

                printTrivia indent "T" isLast token.Token.trailingTrivia

            | _ ->
                
                let tokenMarker = (isLast, []) |> getMarker
                
                setColor System.ConsoleColor.DarkGray
                print "%s" indent
                print "%s" tokenMarker

                setColor System.ConsoleColor.Cyan
                print "%A" node.Kind
                resetColor()
                print "%s" System.Environment.NewLine

                let childIndent =
                    if isLast then
                        indent + "   "
                    else
                        indent + "|  "

                writeChildren childIndent (node.Children |> List.ofSeq)
        write "" true node
        resetColor()

            

    static member FromList (from: _ -> ISyntaxNode) (separatedSyntaxList: SeparatedSyntaxList<_>): ISyntaxNode list =
        match separatedSyntaxList with
        | EmptySyntaxList -> []
        | SeparatedSyntaxList (firstItem, followingItems) ->
            [from firstItem]
            @
            (followingItems
            |> List.collect (fun (token,item) ->
                [ SyntaxNode.From token; from item ]
            ))
            
    static member FromNonEmptyList (from: _ -> ISyntaxNode) (separatedSyntaxList: SeparatedNonEmptySyntaxList<_>): ISyntaxNode list =
        match separatedSyntaxList with
        | SeparatedNonEmptySyntaxList (firstItem, followingItems) ->
            [from firstItem]
            @
            (followingItems
            |> List.collect (fun (token,item) ->
                [ SyntaxNode.From token; from item ]
            ))

    static member FromOption (from: _ -> ISyntaxNode) = function
        | None -> []
        | Some x -> [from x]

    static member FromOptionalType(optionalType) =
        match optionalType with
        | None -> []
        | Some (colon: Token, typeExpression: TypeExpressionSyntax) ->
            [SyntaxNode.From colon; SyntaxNode.From typeExpression]

    static member FromIdWithType(idWithType: IdentifierWithType): _ seq =
        [ SyntaxNode.From idWithType.identifier
          SyntaxNode.From idWithType.colon
          SyntaxNode.From idWithType.requiredType
        ]

    static member From(token: Token) = TokenSyntaxNode(token) :> ISyntaxNode
    static member fromChildren nodeKind children = ChildrenSyntaxNode(nodeKind, children) :> ISyntaxNode
    static member From(this: ModuleDefinitionSyntax) =
        SyntaxNode.fromChildren (ModuleDefinitionKind) [ yield SyntaxNode.From this.moduleName; yield! this.declarations |> Seq.map SyntaxNode.From ]
    static member From(this: ModuleNameSyntax) =
        SyntaxNode.fromChildren ModuleNameKind [ SyntaxNode.From this.moduleKeyword; SyntaxNode.From this.moduleNameIdentifier ]
    static member From(this: IncludeDeclarationSyntax) =
        SyntaxNode.fromChildren
            IncludeDeclarationKind
            [ SyntaxNode.From this.includeKeyword
              SyntaxNode.From this.openParenthesis
              yield! this.cppFileNames |> SyntaxNode.FromList SyntaxNode.From
              SyntaxNode.From this.closeParenthesis
            ]
    static member From(this: DeclarationSyntax) =
        match this with
        | OpenModulesDeclarationSyntax openModulesSyntax -> openModulesSyntax |> SyntaxNode.From
        | AlgebraicTypeSyntax algebraicTypeSyntax -> algebraicTypeSyntax |> SyntaxNode.From
        | FunctionDeclarationSyntax functionDeclarationSyntax -> functionDeclarationSyntax |> SyntaxNode.From
        | LetDeclarationSyntax letDeclarationSyntax -> letDeclarationSyntax |> SyntaxNode.From
        | AliasSyntax aliasSyntax -> aliasSyntax |> SyntaxNode.From
        | InlineCppDeclarationSyntax token ->
            SyntaxNode.fromChildren InlineCppDeclarationKind [SyntaxNode.From token]
        | IncludeDeclarationSyntax includeDeclarationSyntax ->
            SyntaxNode.From includeDeclarationSyntax
    static member From(this: OpenModulesSyntax) =
        SyntaxNode.fromChildren OpenModulesKind
            [ this.openKeyword |> SyntaxNode.From
              this.openParenthesis |> SyntaxNode.From
              yield! this.openedModuleNameIdentifiers |> SyntaxNode.FromList SyntaxNode.From
              this.closeParenthesis |> SyntaxNode.From
            ]
    static member From(this: TemplateDeclarationCapacityIdentifiersSyntax) =
        SyntaxNode.fromChildren TemplateDeclarationCapacityIdentifiersKind
            [ SyntaxNode.From this.semicolon
              yield! this.capacityIdentifiers |> SyntaxNode.FromNonEmptyList SyntaxNode.From
            ]
    static member From(this: TemplateDeclarationSyntax) =
        SyntaxNode.fromChildren TemplateDeclarationKind
            [ SyntaxNode.From this.lessThanSign
              yield! this.typeVariables |> SyntaxNode.FromList SyntaxNode.From
              yield! this.optionalCapacityIdentifiers |> SyntaxNode.FromOption SyntaxNode.From
              SyntaxNode.From this.greaterThanSign
            ]
    static member From(this: CapacityExpressions.CapacityExpressionSyntax) =
        match this with
        | CapacityExpressions.NaturalNumberCapacityExpression naturalToken ->
            SyntaxNode.fromChildren
                NaturalNumberCapacityExpressionKind
                [ SyntaxNode.From naturalToken]
        | CapacityExpressions.BinaryCapacityExpression (left, operator, right) ->
            SyntaxNode.fromChildren
                BinaryCapacityExpressionKind
                [ SyntaxNode.From left
                  SyntaxNode.From operator
                  SyntaxNode.From right
                ]
        | CapacityExpressions.IdentifierCapacityExpression identifierToken ->
            SyntaxNode.fromChildren
                IdentifierCapacityExpressionKind
                [ SyntaxNode.From identifierToken ]
    static member From(this: TemplateApplicationCapacityExpressionsSyntax) =
        SyntaxNode.fromChildren
            TemplateApplicationCapacityExpressionsKind
            [ SyntaxNode.From this.semicolon
              yield! this.capacities |> SyntaxNode.FromNonEmptyList SyntaxNode.From
            ]
    static member From(this: TemplateApplicationSyntax) =
        SyntaxNode.fromChildren
            TemplateApplicationKind
            [ SyntaxNode.From this.lessThanSign
              yield! this.templateApplicationTypes |> SyntaxNode.FromList SyntaxNode.From
              yield! this.optionalCapacityExpressions |> SyntaxNode.FromOption SyntaxNode.From
              SyntaxNode.From this.greaterThanSign
            ]
    static member From(this: AlgebraicTypeSyntax) =
        SyntaxNode.fromChildren
            AlgebraicTypeKind
            [   SyntaxNode.From this.typeKeyword
                SyntaxNode.From this.algebraicTypeIdentifier
                yield! this.optionalTemplateDeclaration |> SyntaxNode.FromOption SyntaxNode.From 
                SyntaxNode.From  this.equals
                yield! this.valueConstructors |> SyntaxNode.FromNonEmptyList SyntaxNode.From  
            ]
    static member From(this: AliasSyntax) =
        SyntaxNode.fromChildren
            AliasKind
            [ SyntaxNode.From this.aliasKeyword
              SyntaxNode.From this.aliasIdentifier
              yield! this.optionalTemplateDeclaration |> SyntaxNode.FromOption SyntaxNode.From
              SyntaxNode.From this.equals
              SyntaxNode.From this.aliasOfType
            ]
    static member From(this: ValueConstructorSyntax) =
        SyntaxNode.fromChildren
            ValueConstructorKind
            [ SyntaxNode.From this.valueConstructorIdentifier
              SyntaxNode.From this.openParenthesis
              yield! this.valueConstructorParameterTypes |> SyntaxNode.FromList SyntaxNode.From
              SyntaxNode.From this.closeParenthesis
            ]
    static member From(this: LetDeclarationSyntax) =
        SyntaxNode.fromChildren
            LetDeclarationKind
            [ SyntaxNode.From this.letKeyword
              SyntaxNode.From this.identifier
              yield! this.optionalType |> SyntaxNode.FromOptionalType
              SyntaxNode.From this.equals
              SyntaxNode.From this.body
            ]
    static member From(this: MemberConstraintSyntax) =
        SyntaxNode.fromChildren
            MemberConstraintKind
            [ SyntaxNode.From this.openBrace
              yield! this.fieldConstraints |> SyntaxNode.FromNonEmptyList (SyntaxNode.FromIdWithType >> SyntaxNode.fromChildren SingleMemberConstraintKind)
              SyntaxNode.From this.closeBrace
            ]
    static member From(this: ConstraintSyntax) =
        match this with
        | NumConstraint numToken ->
            SyntaxNode.fromChildren
                NumConstraintKind
                [ SyntaxNode.From numToken ]
        | IntConstraint intToken ->
            SyntaxNode.fromChildren
                IntConstraintKind
                [ SyntaxNode.From intToken ]
        | RealConstraint realToken ->
            SyntaxNode.fromChildren
                RealConstraintKind
                [ SyntaxNode.From realToken ]
        | PackedConstraint packedToken ->
            SyntaxNode.fromChildren
                PackedConstraintKind
                [ SyntaxNode.From packedToken ]
        | MemberConstraint memberConstraintSyntax ->
            SyntaxNode.From memberConstraintSyntax
    static member From(this: TypeConstraintSyntax) =
        SyntaxNode.fromChildren
            TypeConstraintKind
            [ SyntaxNode.From this.typeExpression
              SyntaxNode.From this.colon
              SyntaxNode.From this.constraint
            ]
    static member From(this: WhereConstraintsSyntax) =
        SyntaxNode.fromChildren
            WhereConstraintsKind
            [ SyntaxNode.From this.whereKeyword
              yield! this.typeConstraints |> SyntaxNode.FromNonEmptyList SyntaxNode.From
            ]
    static member From(this: FunctionDeclarationSyntax) =
        SyntaxNode.fromChildren
            FunctionDeclarationKind
            [ SyntaxNode.From this.funKeyword
              SyntaxNode.From this.identifier
              yield! this.optionalTemplateDeclaration |> SyntaxNode.FromOption SyntaxNode.From
              SyntaxNode.From this.openParenthesis
              yield! this.functionArguments |> SyntaxNode.FromList SyntaxNode.From
              SyntaxNode.From this.closeParenthesis
              yield! this.optionalType |> SyntaxNode.FromOptionalType
              yield! this.optionalWhereConstraints |> SyntaxNode.FromOption SyntaxNode.From
              SyntaxNode.From this.equals
              SyntaxNode.From this.functionBody
            ]
    static member From(this: IdentifierWithOptionalType) =
        SyntaxNode.fromChildren
            IdentifierWithOptionalTypeKind
            [ SyntaxNode.From this.identifier
              yield! this.optionalType |> SyntaxNode.FromOptionalType
            ]
    static member From(this: ModuleQualifierSyntax) =
        SyntaxNode.fromChildren
            ModuleQualifierKind
            [ SyntaxNode.From this.moduleNameIdentifier
              SyntaxNode.From this.colon
              SyntaxNode.From this.moduleDeclarationNameIdentifier
            ]
    static member From(this: IdentifierDeclarationReferenceSyntax) =
        SyntaxNode.fromChildren
            IdentifierDeclarationReferenceKind
            [ SyntaxNode.From this.declarationNameIdentifier ]
    static member From(this: DeclarationReferenceSyntax) =
        match this with
        | IdentifierDeclarationReference identifierDeclarationReferenceSyntax ->
            SyntaxNode.From identifierDeclarationReferenceSyntax
        | ModuleQualifierDeclarationReference moduleQualifierSyntax ->
            SyntaxNode.From moduleQualifierSyntax
    static member From(this: FunctionTypeExpressionSyntax) =
        SyntaxNode.fromChildren
            FunctionTypeExpressionKind
            [ SyntaxNode.From this.closureOpenParenthesis
              SyntaxNode.From this.closureOfFunction
              SyntaxNode.From this.closureCloseParenthesis
              SyntaxNode.From this.argumentTypesOpenParenthesis
              yield! this.argumentTypes |> SyntaxNode.FromList SyntaxNode.From
              SyntaxNode.From this.argumentTypesCloseParenthesis
              SyntaxNode.From this.arrow
              SyntaxNode.From this.returnType
            ]
    static member From(this: ClosureTypeExpressionSyntax) =
        SyntaxNode.fromChildren
            ClosureTypeExpressionKind
            [ SyntaxNode.From this.openPipe
              yield!
                this.capturedVariables
                |> SeparatedSyntaxList.toSeq (SyntaxNode.From >> Seq.singleton) (SyntaxNode.FromIdWithType)
                |> Seq.collect id
              SyntaxNode.From this.closePipe
            ]
    static member From(this: ClosureOfFunctionSyntax): ISyntaxNode =
        match this with
        | ClosureTypeExpressionOfFunction closureTypeExpressionSyntax ->
            SyntaxNode.From closureTypeExpressionSyntax
        | ClosureTypeVariableOfFunction token ->
            SyntaxNode.fromChildren ClosureTypeVariableKind [SyntaxNode.From token]
    static member From(this: RecordTypeExpressionSyntax) =
        SyntaxNode.fromChildren
            RecordTypeExpressionKind
            [ yield! this.packed |> SyntaxNode.FromOption SyntaxNode.From
              SyntaxNode.From this.openBrace
              yield!
                  this.recordMemberTypes 
                  |> SeparatedSyntaxList.toSeq (SyntaxNode.From >> Seq.singleton) (SyntaxNode.FromIdWithType)
                  |> Seq.collect id
              SyntaxNode.From this.closeBrace
            ]
    static member From(this: CapacityTypeExpressionSyntax) =
        SyntaxNode.fromChildren
            CapacityTypeExpressionKind
            [ SyntaxNode.From this.typeExpression
              SyntaxNode.From this.openBracket
              SyntaxNode.From this.capacityExpression
              SyntaxNode.From this.closeBracket
            ]
    static member From(this: RefTypeExpressionSyntax) =
        SyntaxNode.fromChildren
            RefTypeExpressionKind
            [ SyntaxNode.From this.typeExpression
              SyntaxNode.From this.refKeyword
            ]
    static member From(this: TypeExpressionSyntax) =
        match this with
        | DeclarationReferenceTypeExpression (declarationReferenceSyntax, optionalTemplateApplicationSyntax) ->
            SyntaxNode.fromChildren
                DeclarationReferenceTypeExpressionKind
                [ SyntaxNode.From declarationReferenceSyntax
                  yield! optionalTemplateApplicationSyntax |> SyntaxNode.FromOption SyntaxNode.From
                ]
        | BuiltInTypeExpression token ->
            SyntaxNode.fromChildren
                BuiltInTypeExpressionKind
                [ SyntaxNode.From token ]
        | TypeVariableIdentifierTypeExpression token ->
            SyntaxNode.fromChildren
                TypeVariableIdentifierTypeExpressionKind
                [ SyntaxNode.From token ]
        | FunctionTypeExpression functionTypeExpressionSyntax ->
            SyntaxNode.From functionTypeExpressionSyntax
        | ParenthesizedTypeExpressionSyntax (openParenthesis, typeExpression, closeParenthesis) ->
            SyntaxNode.fromChildren
                ParenthesizedTypeExpressionSyntaxKind
                [ SyntaxNode.From openParenthesis
                  SyntaxNode.From typeExpression
                  SyntaxNode.From closeParenthesis
                ]
        | RecordTypeExpression recordTypeExpressionSyntax ->
            SyntaxNode.From recordTypeExpressionSyntax
        | CapacityTypeExpression capacityTypeExpressionSyntax ->
            SyntaxNode.From capacityTypeExpressionSyntax
        | RefTypeExpression refTypeExpressionSyntax ->
            SyntaxNode.From refTypeExpressionSyntax
        | TupleTypeExpression tupleTypeElements ->
            SyntaxNode.fromChildren
                TupleTypeExpressionKind
                [ yield! tupleTypeElements |> SyntaxNode.FromNonEmptyList SyntaxNode.From ]
        | ClosureTypeExpression closureTypeExpressionSyntax ->
            SyntaxNode.From closureTypeExpressionSyntax
    static member From(this: FunctionCallExpressionSyntax) =
        SyntaxNode.fromChildren
            FunctionCallExpressionKind
            [ SyntaxNode.From this.functionExpression
              SyntaxNode.From this.openParenthesis
              yield! this.functionCallArguments |> SyntaxNode.FromList SyntaxNode.From
              SyntaxNode.From this.closeParenthesis
            ]
    static member From(this: IfExpressionSyntax) =
        SyntaxNode.fromChildren
            IfExpressionKind
            [ SyntaxNode.From this.ifKeyword
              SyntaxNode.From this.conditionExpression
              SyntaxNode.From this.thenKeyword
              SyntaxNode.From this.trueExpression
              yield! this.elifBranches |> List.map SyntaxNode.From
              SyntaxNode.From this.elseKeyword
              SyntaxNode.From this.falseExpression
              SyntaxNode.From this.endKeyword
            ]
    static member From(this: ElifBranchSyntax) =
        SyntaxNode.fromChildren
            ElifBranchKind
            [ SyntaxNode.From this.elifKeyword
              SyntaxNode.From this.conditionExpression
              SyntaxNode.From this.thenKeyword
              SyntaxNode.From this.trueExpression
            ]
    static member From(this: LetExpressionSyntax) =
        SyntaxNode.fromChildren
            LetExpressionKind
            [ SyntaxNode.From this.letKeyword
              SyntaxNode.From this.pattern
              SyntaxNode.From this.equals
              SyntaxNode.From this.body
            ]
    static member From(this: LambdaExpressionSyntax) =
        SyntaxNode.fromChildren
            LambdaExpressionKind
            [ SyntaxNode.From this.fnKeyword
              SyntaxNode.From this.openParenthesis
              yield! this.lambdaArguments |> SyntaxNode.FromList SyntaxNode.From
              SyntaxNode.From this.closeParenthesis
              yield! this.optionalReturnType |> SyntaxNode.FromOptionalType
              SyntaxNode.From this.arrow
              SyntaxNode.From this.lambdaBodyExpression
              SyntaxNode.From this.endKeyword
            ]
    static member From(this: CaseOfExpressionSyntax) =
        SyntaxNode.fromChildren
            CaseOfExpressionKind
            [ SyntaxNode.From this.caseKeyword
              SyntaxNode.From this.matchExpression
              SyntaxNode.From this.ofKeyword
              SyntaxNode.From this.firstPipe
              yield! this.caseClauses |> SyntaxNode.FromNonEmptyList SyntaxNode.From
              SyntaxNode.From this.endKeyword
            ]
    static member From(this: ArrayInitializerSyntax) =
        SyntaxNode.fromChildren
            ArrayInitializerKind
            [ SyntaxNode.From this.ofKeyword
              SyntaxNode.From this.initializerExpression
            ]
    static member From(this: IndexerExpressionSyntax) =
        SyntaxNode.fromChildren
            IndexerExpressionKind
            [ SyntaxNode.From this.expression
              SyntaxNode.From this.openBracket
              SyntaxNode.From this.indexExpression
              SyntaxNode.From this.closeBracket
            ]
    static member From(this: MemberAccessExpressionSyntax) =
        SyntaxNode.fromChildren
            MemberAccessExpressionKind
            [ SyntaxNode.From this.expression
              SyntaxNode.From this.dot
              SyntaxNode.From this.identifier
            ]
    static member From(this: TypedExpressionSyntax) =
        SyntaxNode.fromChildren
            TypedExpressionKind
            [ SyntaxNode.From this.expression
              SyntaxNode.From this.colon
              SyntaxNode.From this.typeExpression
            ]
    static member From(this: FieldAssignSyntax) =
        SyntaxNode.fromChildren
            FieldAssignKind
            [ SyntaxNode.From this.fieldNameIdentifier
              SyntaxNode.From this.equals
              SyntaxNode.From this.expression
            ]
    static member From(this: RecordExpressionSyntax) =
        SyntaxNode.fromChildren
            RecordExpressionKind
            [ yield! this.optionalPackedKeyword |> SyntaxNode.FromOption SyntaxNode.From
              SyntaxNode.From this.openBrace
              yield! this.fieldAssigns |> SyntaxNode.FromNonEmptyList SyntaxNode.From
              SyntaxNode.From this.closeBrace
            ]
    static member From(this: ArrayLiteralExpressionSyntax) =
        SyntaxNode.fromChildren
            ArrayLiteralExpressionKind
            [ SyntaxNode.From this.openBracket
              yield! this.elementExpressions |> SyntaxNode.FromList SyntaxNode.From
              SyntaxNode.From this.closeBracket
            ]
    static member From(this: UnsafeTypeCastExpression) =
        SyntaxNode.fromChildren
            UnsafeTypeCastKind
            [ SyntaxNode.From this.expression
              SyntaxNode.From this.colonColonColonColon
              SyntaxNode.From this.typeExpression
            ]
    static member From(this: ExpressionSyntax): ISyntaxNode =
        match this with
        | UnitLiteralExpression (openParenthesis, closeParenthesis) ->
            SyntaxNode.fromChildren UnitLiteralExpressionKind [SyntaxNode.From openParenthesis; SyntaxNode.From closeParenthesis]
        | BoolLiteralExpression token ->
            SyntaxNode.fromChildren BoolLiteralExpressionKind [SyntaxNode.From token]
        | NullLiteralExpression token ->
            SyntaxNode.fromChildren NullLiteralExpressionKind [SyntaxNode.From token]
        | UnaryExpressionSyntax (opToken, expression) ->
            SyntaxNode.fromChildren UnaryExpressionKind [SyntaxNode.From opToken; SyntaxNode.From expression]
        | BinaryExpressionSyntax (leftExpression, opToken, rightExpression) ->
            SyntaxNode.fromChildren BinaryExpressionKind [SyntaxNode.From leftExpression; SyntaxNode.From opToken; SyntaxNode.From rightExpression]
        | IndexerExpression indexerExpression ->
            SyntaxNode.From indexerExpression
        | NumberExpressionSyntax token ->
            SyntaxNode.fromChildren NumberExpressionKind [SyntaxNode.From token]
        | ParenthesizedExpressionSyntax (openParenthesis, expression, closeParenthesis) ->
            SyntaxNode.fromChildren ParenthesizedExpressionKind [SyntaxNode.From openParenthesis; SyntaxNode.From expression; SyntaxNode.From closeParenthesis]
        | SequenceExpression (openBrace, expressions, closeBrace) ->
            SyntaxNode.fromChildren SequenceExpressionKind
                [ SyntaxNode.From openBrace
                  yield! expressions |> SyntaxNode.FromNonEmptyList SyntaxNode.From
                  SyntaxNode.From closeBrace
                ]
        | StringLiteralExpressionSyntax token ->
            SyntaxNode.fromChildren StringLiteralExpressionKind [SyntaxNode.From token]
        | CharacterArrayLiteralExpressionSyntax token ->
            SyntaxNode.fromChildren CharacterArrayLiteralExpressionKind [SyntaxNode.From token]
        | DeclarationReferenceExpressionSyntax (declarationReferenceSyntax, templateApplicationSyntax) ->
            SyntaxNode.fromChildren DeclarationReferenceExpressionKind
                [ SyntaxNode.From declarationReferenceSyntax
                  yield! templateApplicationSyntax |> SyntaxNode.FromOption SyntaxNode.From
                ]
        | CaseOfExpression caseOfExpressionSyntax ->
            SyntaxNode.From caseOfExpressionSyntax
        | InlineCppExpressionSyntax token ->
            SyntaxNode.fromChildren InlineCppExpressionKind [SyntaxNode.From token]
        | FunctionCallExpression functionCallExpressionSyntax ->
            SyntaxNode.From functionCallExpressionSyntax
        | LetExpression letExpressionSyntax ->
            SyntaxNode.From letExpressionSyntax
        | LambdaExpression lambdaExpressionSyntax ->
            SyntaxNode.From lambdaExpressionSyntax
        | IfExpression ifExpressionSyntax ->
            SyntaxNode.From ifExpressionSyntax
        | VariableDeclaration variableDeclaration ->
            SyntaxNode.fromChildren
                VariableDeclarationKind
                [ SyntaxNode.From variableDeclaration.varKeyword
                  SyntaxNode.From variableDeclaration.variableIdentifier
                  SyntaxNode.From variableDeclaration.colonToken
                  SyntaxNode.From variableDeclaration.variableType
                ]
        | SetExpression setExpression ->
            SyntaxNode.fromChildren
                SetExpressionKind
                [ SyntaxNode.From setExpression.setKeyword
                  yield! setExpression.optionalRefKeyword |> SyntaxNode.FromOption SyntaxNode.From 
                  SyntaxNode.From setExpression.leftAssign
                  SyntaxNode.From setExpression.equalsToken
                  SyntaxNode.From setExpression.expression
                ]
        | ForLoopExpression forLoopExpression ->
            SyntaxNode.fromChildren
                ForLoopExpressionKind
                [ SyntaxNode.From forLoopExpression.forKeyword
                  SyntaxNode.From forLoopExpression.identifier
                  yield! forLoopExpression.optionalTyExpr |> SyntaxNode.FromOptionalType
                  SyntaxNode.From forLoopExpression.inKeyword
                  SyntaxNode.From forLoopExpression.startExpression
                  SyntaxNode.From forLoopExpression.direction
                  SyntaxNode.From forLoopExpression.endExpression
                  SyntaxNode.From forLoopExpression.doKeyword
                  SyntaxNode.From forLoopExpression.bodyExpression
                  SyntaxNode.From forLoopExpression.endKeyword
                ]
        | DoWhileExpression doWhileExpression ->
            SyntaxNode.fromChildren
                DoWhileExpressionKind
                [ SyntaxNode.From doWhileExpression.doKeyword
                  SyntaxNode.From doWhileExpression.bodyExpression
                  SyntaxNode.From doWhileExpression.whileKeyword
                  SyntaxNode.From doWhileExpression.conditionExpression
                  SyntaxNode.From doWhileExpression.endKeyword
                ]
        | WhileDoExpression whileDoExpression ->
            SyntaxNode.fromChildren
                WhileDoExpressionKind
                [ SyntaxNode.From whileDoExpression.whileKeyword
                  SyntaxNode.From whileDoExpression.conditionExpression
                  SyntaxNode.From whileDoExpression.doKeyword
                  SyntaxNode.From whileDoExpression.bodyExpression
                  SyntaxNode.From whileDoExpression.endKeyword
                ]
        | MemberAccessExpression memberAccessExpression ->
            SyntaxNode.From memberAccessExpression
        | TypedExpression typedExpression ->
            SyntaxNode.From typedExpression
        | ArrayExpression arrayExpression ->
            SyntaxNode.fromChildren
                ArrayExpressionKind
                [ SyntaxNode.From arrayExpression.arrayKeyword
                  SyntaxNode.From arrayExpression.arrayTypeExpression
                  yield! arrayExpression.optionalInitializerExpression |> SyntaxNode.FromOption SyntaxNode.From
                  SyntaxNode.From arrayExpression.endKeyword
                ]
        | SmartPointerExpression smartPointerExpression ->
            SyntaxNode.fromChildren
                SmartPointerExpressionKind
                [ SyntaxNode.From smartPointerExpression.smartPointerKeyword
                  SyntaxNode.From smartPointerExpression.openParenthesis
                  SyntaxNode.From smartPointerExpression.valueExpression
                  SyntaxNode.From smartPointerExpression.comma
                  SyntaxNode.From smartPointerExpression.destructorExpression
                  SyntaxNode.From smartPointerExpression.closeParenthesis]
        | RecordExpression recordExpressionSyntax ->
            SyntaxNode.From recordExpressionSyntax
        | TupleExpression (openParenthesis, elements, closeParenthesis) ->
            SyntaxNode.fromChildren
                TupleExpressionKind
                [ SyntaxNode.From openParenthesis
                  yield! elements |> SyntaxNode.FromNonEmptyList SyntaxNode.From
                  SyntaxNode.From closeParenthesis]
        | RefExpression (refKeyword, refExpression) ->
            SyntaxNode.fromChildren
                RefExpressionKind
                [ SyntaxNode.From refKeyword
                  SyntaxNode.From refExpression
                ]
        | ArrayLiteralExpression arrayLiteralExpressionSyntax ->
            SyntaxNode.From arrayLiteralExpressionSyntax
        | UnsafeTypeCast unsafeTypeCastExpression ->
            SyntaxNode.From unsafeTypeCastExpression
    static member From(this: ArrayAccessLeftAssignSyntax) =
        SyntaxNode.fromChildren
            ArrayAccessLeftAssignKind
            [ SyntaxNode.From this.arrayLeftAssign
              SyntaxNode.From this.openBracket
              SyntaxNode.From this.indexExpression
              SyntaxNode.From this.closeBracket
            ]
    static member From(this: RecordMemberLeftAssignSyntax) =
        SyntaxNode.fromChildren
            RecordMemberLeftAssignKind
            [ SyntaxNode.From this.recordLeftAssign
              SyntaxNode.From this.memberAccess
              SyntaxNode.From this.memberIdentifier
            ]
    static member From(this: LeftAssignSyntax): ISyntaxNode =
        match this with
        | DeclarationReferenceLeftAssign declarationReference ->
            SyntaxNode.fromChildren DeclarationReferenceLeftAssignKind [SyntaxNode.From declarationReference]
        | ArrayAccessLeftAssign arrayAccess ->
            SyntaxNode.From arrayAccess
        | RecordMemberLeftAssign recordMember ->
            SyntaxNode.From recordMember
    static member From(this: CaseClauseSyntax) =
        SyntaxNode.fromChildren
            CaseClauseKind
            [ SyntaxNode.From this.pattern
              SyntaxNode.From this.doubleArrow
              SyntaxNode.From this.expression
            ]
    static member From(this: VariablePatternSyntax) =
        SyntaxNode.fromChildren
            VariablePatternKind
            [ yield! this.mutableKeyword |> SyntaxNode.FromOption SyntaxNode.From
              SyntaxNode.From this.variablePatternIdentifier
              yield! this.optionalType |> SyntaxNode.FromOptionalType
            ]
    static member From(this: ValConPatternSyntax) =
        SyntaxNode.fromChildren
            ValConPatternKind
            [ SyntaxNode.From this.valConDeclarationReference
              yield! this.optionalTemplateApplication |> SyntaxNode.FromOption SyntaxNode.From
              SyntaxNode.From this.openParenthesis
              yield! this.valConArguments |> SyntaxNode.FromList SyntaxNode.From
              SyntaxNode.From this.closeParenthesis
            ]
    static member From(this: FieldPatternSyntax) = 
        SyntaxNode.fromChildren
            FieldPatternKind
            [ SyntaxNode.From this.fieldIdentifier
              SyntaxNode.From this.equal
              SyntaxNode.From this.fieldPattern]
    static member From(this: RecordPatternSyntax) =
        SyntaxNode.fromChildren
            RecordPatternKind
            [ SyntaxNode.From this.openBrace
              yield! this.fieldPatterns |> SyntaxNode.FromList SyntaxNode.From
              SyntaxNode.From this.closeBrace
            ]
    static member From(this: TuplePatternSyntax) =
        SyntaxNode.fromChildren
            TuplePatternKind
            [ SyntaxNode.From this.openParenthesis
              yield! this.patterns |> SyntaxNode.FromNonEmptyList SyntaxNode.From
              SyntaxNode.From this.closeParenthesis
            ]
    static member From(this: ParenthesizedPatternSyntax) =
        SyntaxNode.fromChildren
            ParenthesizedPatternKind
            [ SyntaxNode.From this.openParenthesis
              SyntaxNode.From this.pattern
              SyntaxNode.From this.closeParenthesis
            ]
    static member From(this: PatternSyntax) =
        match this with
        | VariablePattern variablePatternSyntax ->
            SyntaxNode.From variablePatternSyntax
        | IntegerPattern token ->
            SyntaxNode.fromChildren IntegerPatternKind [SyntaxNode.From token]
        | TruePattern token ->
            SyntaxNode.fromChildren TruePatternKind [SyntaxNode.From token]
        | FalsePattern token ->
            SyntaxNode.fromChildren FalsePatternKind [SyntaxNode.From token]
        | UnderscorePattern token ->
            SyntaxNode.fromChildren UnderscorePatternKind [SyntaxNode.From token]
        | UnitPattern (openParenthesis, closeParenthesis) ->
            SyntaxNode.fromChildren UnitPatternKind [SyntaxNode.From openParenthesis; SyntaxNode.From closeParenthesis]
        | ValConPattern valConPatternSyntax ->
            SyntaxNode.From valConPatternSyntax
        | ParenthesizedPattern parenthesizedPattern ->
            SyntaxNode.From parenthesizedPattern
        | RecordPattern recordPattern ->
            SyntaxNode.From recordPattern
        | TuplePattern tuplePattern ->
            SyntaxNode.From tuplePattern
    static member From(this: SyntaxTree) =
        SyntaxNode.fromChildren
            SyntaxTreeKind
            [ SyntaxNode.From this.expression; SyntaxNode.From this.eofToken ]
