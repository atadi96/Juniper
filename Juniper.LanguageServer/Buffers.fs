module Juniper.LanguageServer.Buffers

type Buffer =
    {
        text: string
        lastAst: (Ast.Module * string) option
        lastTypeCheck: (JuniperCompiler.TypeCheckedProgram * string) option
        compilation: Compilation
    }
and Compilation =
    | SyntaxError
    | TypeError of Ast.Module * Error.ErrorData option
    | Compiled of Ast.Module * JuniperCompiler.TypeCheckedProgram

module Buffer =
    let create text compilation =
        let (ast, typeCheck) =
            match compilation with
            | SyntaxError -> None, None
            | TypeError (ast, _) -> Some (ast, text), None
            | Compiled (ast, typeCheck) -> Some (ast, text), Some (typeCheck, text)
        {
            text = text
            lastAst = ast
            lastTypeCheck = typeCheck
            compilation = compilation
        }

// For more information see https://aka.ms/fsharp-console-apps
type BufferManager() =
    let buffers = System.Collections.Concurrent.ConcurrentDictionary<string,Buffer>()

    member __.UpdateBuffer(documentPath: string, buffer: Buffer) =
        buffers.AddOrUpdate(documentPath, buffer, fun _ _ -> buffer)

    member __.GetBuffer(documentPath: string) =
        match documentPath |> buffers.TryGetValue with
        | true, buffer -> Some buffer
        | _ -> None

    member __.TryGetModule(moduleName: string): Buffer option =
        buffers
            .Values
        |> Seq.tryFind (fun buffer ->
            match buffer.lastAst with
            | Some (moduleDefinition, _) ->
                moduleDefinition
                |> Module.nameInModule
                |> Option.map (Ast.unwrap >> ((=) moduleName))
                |> Option.defaultValue false
            | None -> false
        )
