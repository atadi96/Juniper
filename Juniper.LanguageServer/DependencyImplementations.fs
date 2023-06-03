module Juniper.LanguageServer.DependencyImplementations

open Dependencies

type StandardLibraryModules(stdLibDirectory: string) =
    let stdLibModules =
        StandardLibrary.modules
        |> Seq.map (sprintf "%s/%s.jun" stdLibDirectory)
        |> Seq.map (fun fileName ->
            fileName
            |> System.IO.File.ReadAllText
            |> fun file -> (fileName, file)
            |> JuniperCompiler.syntaxCheck
            |> function
                | Ok ast -> (fileName, ast)
                | _ -> failwith "Error: standard library does not compile!"
        )
        |> List.ofSeq

    let compiledStdLib =
        let (last, rest) =
            stdLibModules
            |> List.rev
            |> function
                | last :: rest -> last, (rest |> List.rev)
        JuniperCompiler.typeCheck rest last
        |> function
            | Ok tc -> tc
            | _ -> failwith "Error: standard library does not compile!"

    interface IStandardLibraryModules with
        member __.GetTypeCheckedStandardLibrary(): JuniperCompiler.TypeCheckedProgram = compiledStdLib
        member __.GetStandardLibraryModules(): (string * Ast.Module) list = stdLibModules
