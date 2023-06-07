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
        JuniperCompiler.typeCheck stdLibModules []
        |> function
            | Ok tc -> tc
            | _ -> failwith "Error: standard library does not compile!"

    interface IStandardLibraryModules with
        member __.GetTypeCheckedStandardLibrary(): JuniperCompiler.TypeCheckedProgram = compiledStdLib
        member __.GetStandardLibraryModules(): (string * Ast.Module) list = stdLibModules

type RootFolderHolder() =
    let mutable rootFolder = None

    interface IRootFolderSetter with
        member this.SetRootFolder(arg1: OmniSharp.Extensions.LanguageServer.Protocol.DocumentUri): unit = 
            rootFolder <- Some arg1

    interface IRootFolderService with
        member this.GetRootFolder(): OmniSharp.Extensions.LanguageServer.Protocol.DocumentUri option = 
            rootFolder
