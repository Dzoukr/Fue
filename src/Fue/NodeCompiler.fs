module Fue.NodeCompiler

open Core
open Data
open StringUtils
open Rop
open FSharp.Data

let private checkIsBool (value:obj) = if(value :? bool) then value :?> bool |> success else ValueExpectedToBeBoolean(value) |> fail
let private bindIf f t b  = if b then t() else f
let private toTuples = List.map (fun (x:HtmlAttribute) -> x.Name(), x.Value())
let private cleanIfAttributes = List.filter (fun (x:HtmlAttribute) -> x.Name() <> "fs-if") >> toTuples
let private toList item = [item]
let private foldResults results =
    let foldFn acc item =
        match acc, item with
        | Success(res), Success(list) -> res @ list |> Success
        | Success(_), Failure(errors) -> errors |> Failure
        | Failure(errs), Failure(newErrs) -> errs @ newErrs |> Failure
        | Failure(errs), Success(_) -> errs |> Failure
    results |> List.fold foldFn (Success [])

let compile data (source:HtmlNode) =
    let rec comp data source  =
        Parser.parseNode(source)
        >>= (fun node ->
            match node with
            | Some(IfCondition(boolValue)) ->
                boolValue |> ValueCompiler.compile data
                >>= checkIsBool
                >>= bindIf ([] |> success) (fun _ ->
                    let attrs = source.Attributes() |> cleanIfAttributes
                    let name = source.Name()
                    source.Elements() |> List.map (comp data) |> foldResults
                    >>=> (fun elms ->
                        HtmlNode.NewElement(name, attrs, elms) |> toList
                    )
                )
            | None ->
                match source.Name() with
                | "" -> 
                    source.DirectInnerText() 
                    |> TemplateCompiler.compile data
                    |> HtmlNode.NewText
                    |> toList |> success
                | name ->
                    source.Elements() |> List.map (comp data) |> foldResults
                    >>=> (fun elms ->
                        HtmlNode.NewElement(name, source.Attributes() |> toTuples, elms) |> toList
                    )
        )
    comp data source