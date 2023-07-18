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
    | TemplateDeclarationKind
    | TemplateApplicationKind
    | ValueConstructorKind
    | FunctionDeclarationKind
    | ModuleQualifierKind
    | IdentifierDeclarationReferenceKind
    | FunctionTypeExpressionKind
    | ClosureTypeExpressionKind
    | IdentifierWithOptionalTypeKind
    | RecordTypeExpressionKind
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
    | VariablePatternKind
    | ValConPatternKind
    | ClosureTypeVariableKind
    | BuiltInTypeExpressionKind
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
    | InlineCppExpressionKind
    | ArrayAccessLeftAssignKind
    | RecordMemberLeftAssignKind
    | DeclarationReferenceLeftAssignKind
    | IntegerPatternKind
    | TruePatternKind
    | FalsePatternKind
    | UnderscorePatternKind
    | UnitPatternKind
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

type SyntaxNode =
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
                        print "%s: %A %A" prefix x.triviaKind x.text
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

                setColor (
                    match token.Token.text with
                    | Some _ -> System.ConsoleColor.Blue
                    | None -> System.ConsoleColor.DarkRed
                )
                print "%A" token.Token.tokenKind
                token.Token.value |> Option.iter (print " %A")
                setColor (
                    match token.Token.tokenKind with
                    | KeywordToken _ -> System.ConsoleColor.DarkYellow
                    | _ -> System.ConsoleColor.White
                )
                token.Token.text |> Option.iter (print " %s")
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

    static member From(token) = TokenSyntaxNode(token) :> ISyntaxNode
    static member fromChildren nodeKind children = ChildrenSyntaxNode(nodeKind, children) :> ISyntaxNode
    static member From(this: ModuleDefinitionSyntax) =
        SyntaxNode.fromChildren (ModuleDefinitionKind) [ yield SyntaxNode.From this.moduleName; yield! this.declarations |> Seq.map SyntaxNode.From ]
    static member From(this: ModuleNameSyntax) =
        SyntaxNode.fromChildren ModuleNameKind [ SyntaxNode.From this.moduleKeyword; SyntaxNode.From this.moduleNameIdentifier ]
    static member From(this: DeclarationSyntax) =
        match this with
        | OpenModulesDeclarationSyntax openModulesSyntax -> openModulesSyntax |> SyntaxNode.From
        | AlgebraicTypeSyntax algebraicTypeSyntax -> algebraicTypeSyntax |> SyntaxNode.From
        | FunctionDeclarationSyntax functionDeclarationSyntax -> functionDeclarationSyntax |> SyntaxNode.From
        | LetDeclarationSyntax letDeclarationSyntax -> letDeclarationSyntax |> SyntaxNode.From
        | AliasSyntax aliasSyntax -> aliasSyntax |> SyntaxNode.From
        | InlineCppDeclarationSyntax token ->
            SyntaxNode.fromChildren InlineCppDeclarationKind [SyntaxNode.From token]
    static member From(this: OpenModulesSyntax) =
        SyntaxNode.fromChildren OpenModulesKind
            [ this.openKeyword |> SyntaxNode.From
              this.openParenthesis |> SyntaxNode.From
              yield! this.openedModuleNameIdentifiers |> SyntaxNode.FromList SyntaxNode.From
              this.closeParenthesis |> SyntaxNode.From
            ]
    static member From(this: TemplateDeclarationSyntax) =
        SyntaxNode.fromChildren TemplateDeclarationKind
            [ SyntaxNode.From this.lessThanSign
              yield! this.typeVariables |> SyntaxNode.FromNonEmptyList SyntaxNode.From
              SyntaxNode.From this.greaterThanSign
            ]
    static member From(this: TemplateApplicationSyntax) =
        SyntaxNode.fromChildren
            TemplateApplicationKind
            [ SyntaxNode.From this.lessThanSign
              yield! this.templateApplicationTypes |> SyntaxNode.FromNonEmptyList SyntaxNode.From
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
        | ClosureTypeExpression closureTypeExpressionSyntax ->
            SyntaxNode.From closureTypeExpressionSyntax
        | ClosureTypeVariable token ->
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
    static member From(this: TypeExpressionSyntax) =
        match this with
        | DeclarationReferenceTypeExpression declarationReferenceSyntax ->
            SyntaxNode.From declarationReferenceSyntax
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
                  SyntaxNode.From setExpression.leftAssign
                  SyntaxNode.From setExpression.equalsToken
                  SyntaxNode.From setExpression.expression
                ]
        | SetRefExpression setRefExpression ->
            SyntaxNode.fromChildren
                SetRefExpressionKind
                [ SyntaxNode.From setRefExpression.setKeyword
                  SyntaxNode.From setRefExpression.refKeyword
                  SyntaxNode.From setRefExpression.leftAssign
                  SyntaxNode.From setRefExpression.equalsToken
                  SyntaxNode.From setRefExpression.expression
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
    static member From(this: LeftAssignSyntax) =
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
    static member From(this: SyntaxTree) =
        SyntaxNode.fromChildren
            SyntaxTreeKind
            [ SyntaxNode.From this.expression; SyntaxNode.From this.eofToken ]
