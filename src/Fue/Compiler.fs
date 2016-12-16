module Fue.Compiler

open FSharp.Data
open Rop

let private rootName = "fue-root"
let private rootOpenTag = sprintf "<%s>" rootName
let private rootCloseTag = sprintf "</%s>" rootName

let private addSafeTags str = rootOpenTag + str + rootCloseTag
let private removeSafeTags (str:string) = 
    let start = rootOpenTag.Length
    let length = str.Length - (rootOpenTag.Length + rootCloseTag.Length)
    str.Substring(start, length)

let private safeParse str =
    try
        false, HtmlNode.Parse str
    with :? System.Exception -> true, str |> addSafeTags |> HtmlNode.Parse


let compileFromString data str =
    let removeTags, value = str |> safeParse
    value
    |> List.map (NodeCompiler.compile data)
    |> Rop.fold
    >>=> List.fold (fun a i -> a + i.ToString()) ""
    |> Rop.extract
    |> (fun value -> if removeTags then value |> removeSafeTags else value)