module Juniper.LanguageServer.Dependencies

open OmniSharp.Extensions.LanguageServer.Protocol.Models
open OmniSharp.Extensions.LanguageServer.Protocol

type IStandardLibraryModules =
    abstract GetStandardLibraryModules : unit -> (string * Ast.Module) list
    abstract GetTypeCheckedStandardLibrary : unit -> JuniperCompiler.TypeCheckedProgram
    
type IRootFolderSetter =
    abstract SetRootFolder : DocumentUri -> unit

type IRootFolderService =
    abstract GetRootFolder : unit -> DocumentUri option
    
let junDocumentSelector =
    let docuFilter = DocumentFilter()
    docuFilter.Pattern <- "**/*.jun"
    DocumentSelector(docuFilter)