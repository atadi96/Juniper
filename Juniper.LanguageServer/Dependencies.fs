module Juniper.LanguageServer.Dependencies

open OmniSharp.Extensions.LanguageServer.Protocol.Models

type IStandardLibraryModules =
    abstract GetStandardLibraryModules : unit -> (string * Ast.Module) list
    abstract GetTypeCheckedStandardLibrary : unit -> JuniperCompiler.TypeCheckedProgram
    
    
let junDocumentSelector =
    let docuFilter = DocumentFilter()
    docuFilter.Pattern <- "**/*.jun"
    DocumentSelector(docuFilter)