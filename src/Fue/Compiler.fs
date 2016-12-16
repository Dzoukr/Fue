module Fue.Compiler

open FSharp.Data
open Rop

let compileFromString data str =
    HtmlNode.Parse str
    |> List.map (NodeCompiler.compile data)
    |> Rop.fold
    >>=> List.fold (fun a i -> a + (i |> string)) ""
    |> Rop.extract