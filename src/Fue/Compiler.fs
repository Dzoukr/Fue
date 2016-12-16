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
    let html = """ 
    <!DOCTYPE html>
<html lang="en">
<head>
    <title>Test</title>
</head>
<body>
    Hi
</body>
</html>
    """
    
    let doc = html |> HtmlDocument.Parse
    let typ = doc |> HtmlDocument.docType 
    let newDoc = HtmlDocument.New(typ, doc.Elements())
    let newDocString = newDoc.ToString()
    
    
    
    let doc = str |> HtmlDocument.Parse
    let typ = doc |> HtmlDocument.docType 



    addedSafeTags, (doc.Elements()), typ

let private safeParse str =
    try
        str |> parse false
    with :? System.Exception -> str |> addSafeTags |> parse true

let compileFromString data str =
    let removeTags, value, docType = str |> safeParse
    value
    |> List.map (NodeCompiler.compile data)
    |> Rop.fold
    >>=> asDocument docType
    >>=> documentToString
    >>=> (fun value -> if removeTags then value |> removeSafeTags else value)
    |> Rop.extract

let private getFullPath file =
    match Path.IsPathRooted file with
    | true -> file
    | false -> Path.Combine([|AppDomain.CurrentDomain.BaseDirectory; file|])

let compileFromFile data file =
    let content = file |> getFullPath |> File.ReadAllText
    content |> compileFromString data