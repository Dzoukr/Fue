module Fue.Compiler

open System.Text.RegularExpressions
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

let private _fromText compiler str =
    let template =
        str
        |> getChildNodes
        |> Seq.toList

    fun data ->
        template
        |> List.map (compiler data)
        |> Rop.fold
        <!> asDocument
        |> extract

/// Compiles text
let fromText = _fromText NodeCompiler.compile
/// Compiles text with escaping dangerous chars
let fromTextSafe = _fromText NodeCompiler.compileSafe

/// Compiles file content
let fromFile file =
    let content = File.ReadAllText (file |> getFullPath)
    fromText content

/// Compiles file content with escaping dangerous chars
let fromFileSafe file =
    let content = File.ReadAllText (file |> getFullPath)
    fromTextSafe content

/// Compiles none html text
let fromNoneHtmlText (str: string) map' =
    let troubleMakers = [
        ("<", "&gt;")
        (">", "&lt;")
    ]
    
    let toNoneTroubledString (str': string) =
        troubleMakers
        |> List.fold (fun (s: string) -> s.Replace) str'
    
    let toTroubledString (str': string) =
        troubleMakers
        |> List.fold (fun (s: string) (t1, t2) -> s.Replace(t2, t1)) str'
        
    let returnFsCommands (str': string) =
        Regex.Matches(str', @"(?<gt>&gt;/?)\s*?(?:fs-\w+).+?(?<lt>&lt;)")
        |> Seq.cast
        |> Seq.fold (fun (acc: string) (m: Match) ->
            m.Value
            |> toTroubledString
            |> (fun s -> acc.Replace(m.Value, s))
        ) str'

    let compile str' = _fromText NodeCompiler.compile str' map'                
                
    str
    |> toNoneTroubledString
    |> returnFsCommands
    |> compile
    |> toTroubledString
        
/// Compiles none html file content
let fromNoneHtmlFile file =
    let content = File.ReadAllText (file |> getFullPath)
    fromNoneHtmlText content    