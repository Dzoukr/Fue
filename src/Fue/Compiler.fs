module Fue.Compiler

open FSharp.Data
open Rop
open System.IO
open System

let private rootName = "fue-root"
let private rootOpenTag = sprintf "<%s>" rootName
let private rootCloseTag = sprintf "</%s>" rootName

let private addSafeTags str = rootOpenTag + str + rootCloseTag
let private removeSafeTags (str:string) = 
    let start = rootOpenTag.Length
    let length = str.Length - (rootOpenTag.Length + rootCloseTag.Length)
    str.Substring(start, length)

let private asDocument docType elms = HtmlDocument.New(docType, elms)

let private documentToString doc = doc.ToString()

let private parse addedSafeTags str = 
    let doc = str |> HtmlDocument.Parse
    let typ = doc |> HtmlDocument.docType 
    addedSafeTags, (doc.Elements()), typ

let private safeParse str =
    try
        str |> parse false
    with :? System.Exception -> str |> addSafeTags |> parse true

let private getFullPath file =
    match Path.IsPathRooted file with
    | true -> file
    | false -> Path.Combine([|AppDomain.CurrentDomain.BaseDirectory; file|])

let private extract = function
    | Success(res) -> res
    | Failure(errors) -> 
        errors 
        |> List.map (Rop.explain) 
        |> List.fold (fun a i -> a + ", " + i) ""
        |> sprintf "Compilation errors found: %s" 

/// Compiles text
let fromText str data =
    let removeTags, value, docType = str |> safeParse
    value
    |> List.map (NodeCompiler.compile data)
    |> Rop.fold
    >>=> asDocument docType
    >>=> documentToString
    >>=> (fun value -> if removeTags then value |> removeSafeTags else value)
    |> extract

/// Compiles file content
let fromFile file data =
    let content = file |> getFullPath |> File.ReadAllText
    data |> fromText content