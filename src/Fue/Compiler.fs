module Fue.Compiler

open Rop
open System.IO
open System
open HtmlAgilityPack
open Extensions

let private asDocument elms = 
    let doc = HtmlDocument.Create()
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

let private _fromText compiler str data =
    str
    |> getChildNodes
    |> Seq.toList
    |> List.map (compiler data)
    |> Rop.fold
    <!> asDocument
    |> extract

/// Compiles text
let fromText = _fromText NodeCompiler.compile
/// Compiles text with escaping dangerous chars
let fromTextSafe = _fromText NodeCompiler.compileSafe

/// Compiles file content
let fromFile file data = 
    let content = File.ReadAllText (file |> getFullPath)
    data |> fromText content

/// Compiles file content with escaping dangerous chars
let fromFileSafe file data = 
    let content = File.ReadAllText (file |> getFullPath)
    data |> fromTextSafe content
