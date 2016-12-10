module Fue.Runner

open Core
open Data
open StringUtils
open Rop
open FSharp.Data

let private checkIsBool (value:obj) = if(value :? bool) then value :?> bool |> success else ValueExpectedToBeBoolean(value) |> fail
let private bindIf f t b  = if b then t() else f
let private createNode (source:HtmlNode) = [source]

let cleanIfAttributes = List.filter (fun (x:HtmlAttribute) -> x.Name() <> "fs-if")

let replaceText data text = text

let todo (source:HtmlNode) (node:TemplateNode) data =
    match node with
    | IfCondition(boolValue) -> 
        boolValue |> ValueCompiler.compile data
        >>= checkIsBool
        // get inner text (with replacement on current level)
        // get cleaned attributes
        >>=> bindIf [] (fun _ ->
            let attrs = source.Attributes() |> cleanIfAttributes
            let text = source.DirectInnerText() |> replaceText data
            createNode(source)
        )
