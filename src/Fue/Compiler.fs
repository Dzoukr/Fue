module Fue.Compiler

open Rop
open System.IO
open System
open HtmlAgilityPack
open Extensions

let private asDocument elms = 
    let doc = HtmlDocument()
    elms |> List.iter (doc.DocumentNode.AppendChild >> ignore)
    match doc.DocumentNode.ChildNodes.Count with
    | 0 -> ""
    | _ -> doc.DocumentNode.OuterHtml

let private getFullPath file =
    match Path.IsPathRooted file with
    | true -> file
    | false -> Path.Combine([|AppDomain.CurrentDomain.BaseDirectory; file|])

let private extract = function
    | Success(res) -> res
    | Failure(errors) -> 
        errors 
        |> List.map (Rop.explain) 
        |> List.fold (fun a i -> a + i + ",") ""
        |> sprintf "Compilation errors found: %s" 

let private getChildNodes str = (str |> HtmlDocument.ParseNode).ChildNodes

/// Compiles text
let fromText str data =
    str
    |> getChildNodes
    |> Seq.toList
    |> List.map (NodeCompiler.compile data)
    |> Rop.fold
    >>=> asDocument
    |> extract

/// Compiles file content
let fromFile file data =
    let content = file |> getFullPath |> File.ReadAllText
    data |> fromText content