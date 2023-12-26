module Parse

type Syntax = Syntax of Token list * SyntaxRule list

and Token = Token of string * string

and SyntaxRule = SyntaxRule of (string * (* Modifier option * *) Substitution list)

(* and Modifier =
    | BinaryOperator *)

and Substitution = Substitution of string * (SyntaxElement list)

and SyntaxElement =
    | Literal of string
    | Rule of string option * string
    | Optional of SyntaxElement list
    | Many of SyntaxElement list

open FParsec

let id = identifier (IdentifierOptions())

let parseToken =
    skipString "token" >>. spaces >>. skipChar '"' >>. charsTillString "\"" true System.Int32.MaxValue .>> spaces .>>. id .>> spaces .>> skipString ";" |>> Token

let (parseSyntaxElement, parseSyntaxElementRef) = createParserForwardedToRef()

let parseSubstitution : Parser<_, unit> =
    let parseSyntaxElement = parseSyntaxElement .>> spaces
    id .>> spaces .>>. many1 (parseSyntaxElement) |>> Substitution

parseSyntaxElementRef.Value <-
    choice [
        skipChar '"' >>. charsTillString "\"" true System.Int32.MaxValue |>> Literal
        skipChar '<' >>. opt (attempt (id .>> spaces .>> skipString ":" .>> spaces)) .>>. charsTillString ">" true System.Int32.MaxValue |>> Rule
        skipChar '[' >>. spaces >>. many1 (parseSyntaxElement .>> spaces) .>> skipChar ']' |>> Optional
        skipChar '{' >>. spaces >>. many1 (parseSyntaxElement .>> spaces) .>> skipChar '}' |>> Many
    ]

let parseSyntaxRule =
    pipe3
        (skipChar '<' >>. charsTillString ">" true System.Int32.MaxValue .>> spaces .>> skipString "::=" .>> spaces)
        (parseSubstitution .>> spaces)
        (many (skipChar '|' >>. spaces >>. parseSubstitution) .>> skipChar ';' .>> spaces)
        (fun name first rest -> SyntaxRule (name, first :: rest))

let parseSyntax =
    spaces >>. many (parseToken .>> spaces) .>>. many parseSyntaxRule .>> spaces .>> eof |>> Syntax

module Seq =
    let intersparse separator (elements: _ seq) =
        let iterator = elements.GetEnumerator()
        seq {
            if iterator.MoveNext() then
                yield iterator.Current
            while iterator.MoveNext() do
                yield separator
                yield iterator.Current
        }

module Naming =
    let toLowerStart str =
        str
        |> String.mapi (fun i c -> if i = 0 then System.Char.ToLower c else c)

    let toUpperStart str =
        str
        |> String.mapi (fun i c -> if i = 0 then System.Char.ToUpper c else c)

    let variable parts =
        match parts with
        | [] -> ""
        | x :: xs ->
            String.concat "" (toLowerStart x :: (xs |> List.map toUpperStart))
    
    let typeName parts =
        parts
        |> List.map toUpperStart
        |> String.concat ""

    let fromRuleName (ruleName: string) =
        ruleName.Split('-')
        |> Seq.mapi (fun i c ->
            if i = 0 then
                c
            else
                c |> String.mapi (fun i c -> if i = 0 then System.Char.ToUpper c else c)
        )
        |> Seq.toList
        |> String.concat ""

type TokenName =
    | TokenName of string
    | KeywordName of string

type SyntaxElementType =
    | IdentifierType of string option
    | TokenType of TokenName
    | RuleType of string option * string
    | OptionType of SyntaxElementType
    | ListType of SyntaxElementType
    | SeparatedNonEmpty of TokenName * SyntaxElementType
    | Separated of TokenName * SyntaxElementType
    | TupleType of SyntaxElementType list
    
type SubstitutionIR = SubstitutionIR of string * SyntaxElementType list
type SyntaxRuleIR = SyntaxRuleIR of string * SubstitutionIR list

module SyntaxElementType =
    let private tokenName tokenLookup l =
        match l |> tokenLookup with
        | Some tokenName ->
            TokenName tokenName
        | None ->
            match l with
            | "identifier" -> TokenName "identifier"
            | _ ->
                if l |> Seq.take 1 |> Seq.forall System.Char.IsLetter then
                    l + "Keyword" |> KeywordName
                else
                    failwithf "Token '%s' is not a keyword, and isn't declared" l

    let rec getType (tokenLookup: string -> string option) (syntaxElement: SyntaxElement) =
        match syntaxElement with
        | Literal l -> TokenType (tokenName tokenLookup l)
        | Rule (name, "id") -> IdentifierType name
        | Rule (name, rule) ->
            RuleType (name, Naming.fromRuleName rule)
        | Optional substitution ->
            match substitution |> getSubstitutionType tokenLookup with
            | SeparatedNonEmpty (token, rule) ->
                Separated (token, rule)
            | ListType _ as listType ->
                listType
            | other -> OptionType other
        | Many substitution -> substitution |> getSubstitutionType tokenLookup |> ListType

    and getSubstitutionType (tokenLookup: string -> string option) (elements): SyntaxElementType =
        match elements with
        | [singleElement] -> getType tokenLookup singleElement
        | [] -> TupleType []
        | _ ->
            let rec detectSeparateds elements =
                match elements with
                | Rule (name, ruleType) :: Many ([Literal l; Rule (_, ruleType')]) :: rest when ruleType = ruleType' ->
                    SeparatedNonEmpty (tokenName tokenLookup l, RuleType (name, Naming.fromRuleName ruleType)) :: detectSeparateds rest
                | x :: xs ->
                    getType tokenLookup x :: detectSeparateds xs
                | [] -> []
            match detectSeparateds elements with
            | [singleElement] -> singleElement
            | convertedElements -> TupleType convertedElements

    let rec getAstType (syntaxElementType: SyntaxElementType) =
        match syntaxElementType with
        | IdentifierType _
        | TokenType _ -> "Token"
        | RuleType (_, rule) -> [ rule; "Syntax" ] |> Naming.typeName
        | OptionType o -> sprintf "%s option" (o |> getAstType)
        | ListType l -> sprintf "%s list" (l |> getAstType)
        | SeparatedNonEmpty (_token, s) ->
            sprintf "SeparatedNonEmptySyntaxList<%s>" (s |> getAstType)
        | Separated (_token, s) ->
            sprintf "SeparatedSyntaxList<%s>" (s |> getAstType)
        | TupleType t ->
            sprintf "(%s)" (t |> Seq.map getAstType |> String.concat " * ")

    let rec getName (syntaxElementType: SyntaxElementType) =
        match syntaxElementType with
        | IdentifierType None ->
            "identifier"
        | IdentifierType (Some identifierName) ->
            [ identifierName ]
            |> Naming.variable
        | TokenType (TokenName tokenName) ->
            [ tokenName ]
            |> Naming.variable
        | TokenType (KeywordName keyword) ->
            [ keyword ]
            |> Naming.variable
        | RuleType (Some name, _) -> [ name ] |> Naming.variable
        | RuleType (None, rule) ->
            rule |> sprintf "_%s_"
        | OptionType o ->
            [ "optional"; getName o ]
            |> Naming.variable
        | SeparatedNonEmpty (_, rule)
        | Separated (_, rule)
        | ListType rule ->
            sprintf "%ss" (rule |> getName)
        | TupleType elements ->
            elements
            |> Seq.map (getName)
            |> Seq.intersparse "With"
            |> Seq.toList
            |> Naming.variable

    let rec getFirstToken (ruleLookup: string -> SubstitutionIR list option) (syntaxElementType: SyntaxElementType) =
        let getFirstToken = getFirstToken ruleLookup
        match syntaxElementType with
        | IdentifierType _ -> Some (TokenName "identifier")
        | TokenType token -> Some token
        | RuleType (_, rule) ->
            match rule |> ruleLookup with
            | Some [ SubstitutionIR (_, singleSubElements) ] -> singleSubElements |> List.tryHead |> Option.bind getFirstToken
            | Some [] -> None
            | Some _ -> None //failwith "can't determine first token in union"
            | None -> failwithf "no rule such as %s" rule
        | OptionType optional -> optional |> getFirstToken
        | Separated (_sepToken, parser) -> parser |> getFirstToken
        | SeparatedNonEmpty (_sepToken, parser) ->
            parser |> getFirstToken
        | ListType inner -> inner |> getFirstToken
        | TupleType elements ->
            elements
            |> List.tryHead
            |> Option.bind getFirstToken

    let getTokenName token =
        match token with
        | TokenName tokenName -> [ tokenName; "Token" ] |> Naming.typeName
        | KeywordName keywordName ->
            sprintf "(KeywordToken %s)" (Naming.typeName [ keywordName ])

    let rec getParser (getFirstToken: SyntaxElementType -> TokenName option) (syntaxElementType: SyntaxElementType) =
        let getParser = getParser getFirstToken
        match syntaxElementType with
        | IdentifierType _ -> TokenType (TokenName "identifier") |> getParser
        | TokenType token -> sprintf "matchToken %s" (getTokenName token)
        | RuleType (_, rule) -> [ "parse"; rule ] |> Naming.variable
        | OptionType optional ->
            let firstTokenText =
                optional
                |> getFirstToken
                |> Option.map getTokenName
                |> Option.defaultValue "MISSING_FIRST_TOKEN"
            sprintf "%s |> ifCurrentKind %s" (optional |> getParser) firstTokenText
        | Separated (sepToken, parser) ->
            sprintf "manySep %s %s MISSING_END_TOKEN" (getParser parser) (getTokenName sepToken)
        | SeparatedNonEmpty (sepToken, parser) ->
            sprintf "many1Sep %s %s" (getParser parser) (getTokenName sepToken)
        | ListType element -> sprintf "many (%s)" (getParser element)
        | TupleType _ -> "WIP tuple"

    let rec getSyntaxNode (syntaxElementType: SyntaxElementType) =
        match syntaxElementType with
        | IdentifierType _
        | TokenType _
        | RuleType _ -> sprintf "SyntaxNode.From this.%s" (syntaxElementType |> getName)
        | ListType _ -> sprintf "yield! this.%s |> List.map SyntaxNode.From" (syntaxElementType |> getName)
        | OptionType _ -> sprintf "yield! this.%s |> SyntaxNode.FromOption SyntaxNode.From" (syntaxElementType |> getName)
        | Separated _ -> sprintf "yield! this.%s |> SyntaxNode.FromList SyntaxNode.From" (syntaxElementType |> getName)
        | SeparatedNonEmpty _ -> sprintf "yield! this.%s |> SyntaxNode.FromNonEmptyList SyntaxNode.From" (syntaxElementType |> getName)
        | TupleType _ -> sprintf "yield! this.%s |> SyntaxNode.From" (syntaxElementType |> getName)

let getIR (Syntax (tokens, rules)) =
    let resolveTokenName tokenName =
        let tokens =
            tokens
            |> Seq.map (fun (Token (x,y)) -> x,y)
            |> Map.ofSeq
        tokens |> Map.tryFind tokenName
    rules
    |> List.map (fun (SyntaxRule (name, substList)) ->
        let substIR =
            substList
            |> List.map (fun (Substitution (substName, elements)) ->
                let elementsIR =
                    elements
                    |> SyntaxElementType.getSubstitutionType resolveTokenName
                    |> function
                        | TupleType x -> x
                        | x -> [ x ]
                SubstitutionIR (substName, elementsIR)
            )
        SyntaxRuleIR (Naming.fromRuleName name, substIR)
    )

let indent lines =
    lines |> Seq.map (sprintf "    %s")
let printMatch varName cases defaultCase =
    seq {
        yield sprintf "match %s with" varName
        for (pattern, value) in cases do
            yield sprintf "| %s ->" pattern
            yield! value |> indent
        match defaultCase with
        | Some defaultExpression ->
            yield sprintf "| _ ->"
            yield! defaultExpression |> indent
        | None -> ()
    }
let printUnion typeName cases =
    seq {
        yield sprintf "type %s =" typeName
        for (caseName, caseType) in cases do
            yield! [ sprintf "| %s of %s" caseName caseType ] |> indent
    }

let generateLeftRecursion ruleLookup (rule: SyntaxRuleIR) =
    let (SyntaxRuleIR (name, substitutions)) = rule
    let leftRecursiveSubsts =
        substitutions
        |> List.choose (fun (SubstitutionIR (substName, elements)) ->
            match elements with
            | RuleType (_, leftName) :: (TokenType tokenName) :: _ when name = leftName ->
                Some (substName, tokenName, elements)
            | _ ->
                None
        )
    if not leftRecursiveSubsts.IsEmpty then
        let typeName = [ name; "LeftRecursion" ] |> Naming.typeName
        printfn "type private %s =" typeName
        for (substName, _firstToken, elements) in leftRecursiveSubsts do
            let substCaseName = [ substName; "LeftRecursion" ] |> Naming.typeName
            printfn "    | %s of %s" substCaseName (elements |> List.skip 1 |> TupleType |> SyntaxElementType.getAstType)

        let oneLeftRecursionName = ["tryParseOne"; name; "LeftRecursion"] |> Naming.variable

        printfn "let %s =" oneLeftRecursionName
        printfn "    par {"
        printfn "        match! currentKind with"
        for (substName, firstTokenName, elements) in leftRecursiveSubsts do
            let substCaseName = [ substName; "LeftRecursion" ] |> Naming.typeName
            let firstToken = firstTokenName |> SyntaxElementType.getTokenName
            printfn "        | %s ->" (firstToken)
            let printParser element parser =
                printfn
                    "            let! %s = %s"
                    (element |> SyntaxElementType.getName)
                    (parser |> Option.defaultValue (element |> SyntaxElementType.getParser (SyntaxElementType.getFirstToken ruleLookup)))
            printParser (TokenType firstTokenName) (Some "nextToken")
            for restElement in elements |> List.skip 2 do
                printParser restElement None
            printfn "            let leftRecursion = %s (%s)" substCaseName (elements |> Seq.skip 2 |> Seq.map SyntaxElementType.getName |> String.concat ", ")
            printfn "            return Some leftRecursion"
        printfn "        | _ ->"
        printfn "            return None"
        printfn "    }"
        printfn "par {"
        printfn "    let! %s = %s" (["primary"; name] |> Naming.variable) (["parsePrimary"; name] |> Naming.variable)
        printfn "    let! leftRecursion = many %s" oneLeftRecursionName
        printfn "    return"
        printfn "        leftRecursion"
        printfn "        |> List.fold (fun %s lRec ->" (["folding"; name] |> Naming.variable)
        printfn "            match lRec with"
        for (substName,_firstTokenName,elements) in leftRecursiveSubsts do
            let substCaseName = [ substName; "LeftRecursion" ] |> Naming.typeName
            printfn "            | %s (%s) ->" substCaseName (elements |> Seq.skip 1 |> Seq.map SyntaxElementType.getName |> String.concat ", ")
            printfn "                %s {" ([ substName; name ] |> Naming.typeName)
            printfn "                    %s = %s" (elements |> List.head |> SyntaxElementType.getName) (["folding"; name] |> Naming.variable)
            for trailElement in (elements |> Seq.skip 1) do
                printfn "                    %s = %s" (trailElement |> SyntaxElementType.getName) (trailElement |> SyntaxElementType.getName)
            printfn "                }"
        printfn "}"

let asdSyntax (syntax) =
    let rules = syntax |> getIR
    let ruleLookup =
        rules
        |> Seq.map (fun (SyntaxRuleIR (name, substs)) -> name, substs)
        |> Map.ofSeq
        |> fun map key -> map |> Map.tryFind key

    let generateRecord name (SubstitutionIR (_, singleSubstitution)) =
        printfn "type %s =" ([ name; "Syntax" ] |> Naming.typeName)
        printfn "    {"
        let subst =
            singleSubstitution
        for memberType in subst do
            printf "        "
            let name =
                memberType
                |> SyntaxElementType.getName
            let memberTypeName = memberType |> SyntaxElementType.getAstType
            printfn "%s: %s" name memberTypeName
        printfn "    }"
        printfn ""

    let generateParser name (SubstitutionIR (_, singleSubstitution)) =
        printfn "let %s: Parser<%s> =" ([ "parse"; name ] |> Naming.variable) ([ name; "Syntax" ] |> Naming.typeName)
        printfn "    par {"
        let subst =
            singleSubstitution
        for memberType in subst do
            printf "        "
            let name =
                memberType
                |> SyntaxElementType.getName
            let memberTypeName = memberType |> SyntaxElementType.getParser (SyntaxElementType.getFirstToken ruleLookup)
            printfn "let! %s = %s" name memberTypeName
        printfn "        return {"
        for memberType in subst do
            printf "            "
            let name =
                memberType
                |> SyntaxElementType.getName
            printfn "%s = %s" name name
        printfn "        }"
        printfn "    }"
        printfn ""

    let generateRecordSyntaxNode name (SubstitutionIR (_, singleSubstitution)) =
        printfn "static member From(this: %s) =" ([name; "Syntax"] |> Naming.typeName)
        printfn "    SyntaxNode.fromChildren"
        printfn "        %s" ([name; "Kind"] |> Naming.typeName)
        match singleSubstitution with
        | first :: rest ->
            printfn "        [ %s" (first |> SyntaxElementType.getSyntaxNode)
            for x in rest do
                printfn "          %s" (x |> SyntaxElementType.getSyntaxNode)
            printfn "        ]"
        | [] ->
            printfn "        []"
            
    let condition (SyntaxRuleIR (name, _)) = name = "constraint"

    // AST
    rules
    |> Seq.where condition
    |> Seq.iter (function
        | (SyntaxRuleIR (name, [singleSubstitution])) -> 
            generateRecord name singleSubstitution
        | (SyntaxRuleIR (name, substitutions)) ->
            for SubstitutionIR (substName, substElements) as subst in substitutions do
                generateRecord ([ substName; name ] |> Naming.typeName) subst
            printfn "type %s =" ([ name; "Syntax" ] |> Naming.typeName)
            for SubstitutionIR (substName, substElements) in substitutions do
                printf "    "
                let typeName = [ substName; name; "Syntax" ] |> Naming.typeName
                let substCase = [ substName; name ] |> Naming.typeName
                printfn "| %s of %s" substCase typeName
            printfn ""
    )

    // PARSER
    rules
    |> Seq.where condition
    |> Seq.iter (function
        | (SyntaxRuleIR (name, [singleSubstitution])) -> 
            generateParser name singleSubstitution
        | (SyntaxRuleIR (name, substitutions)) as rule ->
            rule |> generateLeftRecursion ruleLookup
            for SubstitutionIR (substName, substElements) as subst in substitutions do
                generateParser ([ substName; name ] |> Naming.typeName) subst
    )

    // SYNTAXNODE
    rules
    |> Seq.where condition
    |> Seq.iter (function
        | SyntaxRuleIR (name, [singleSubstitution]) ->
            generateRecordSyntaxNode name singleSubstitution
        | _ -> ()
    )
