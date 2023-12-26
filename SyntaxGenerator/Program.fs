// For more information see https://aka.ms/fsharp-console-apps
open FParsec

printfn "Hello from SyntaxGenerator"
[<EntryPoint>]
let main args =
    let filename = @"C:\Users\atadi\source\repos\Juniper\juniper-repo\grammar.bnf"
    let result = FParsec.CharParsers.runParserOnFile Parse.parseSyntax () filename System.Text.Encoding.UTF8
    match result with
    | Success (s, _, _) ->
        printfn "%A" s
        printfn ""
        Parse.asdSyntax s
    | _ -> printfn "%A" result
    0