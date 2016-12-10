module Fue.Parser

open System
open Core
open StringUtils
open System.Text.RegularExpressions
open FSharp.Data
open Rop

let private (==>) regex value =
    let regex = new Regex(regex, RegexOptions.IgnoreCase ||| RegexOptions.Singleline)
    regex.Match(value).Groups

let private toFunctionParams t = t |> split ',' |> List.map clean
let private splitByCurrying t = 
    let f,s = t |> splitToFirstAndList ' '
    f, (s |> List.map SimpleValue)

let private (|TwoPartsMatch|_|) (groups:GroupCollection) =
    match groups.Count with
    | 3 ->
        let fnName = groups.[1].Value |> clean
        let parts = groups.[2].Value |> clean
        (fnName, parts) |> Some
    | _ -> None

let parseTemplateValue text =
    let rec parse t =
        match "(.+)\|\>(.+)" ==> t with
        | TwoPartsMatch(parts, fnName) ->
            let f,p = fnName |> splitByCurrying
            Function(f, p @ [parts |> parse])
        | _ -> 
            match "(.+?)\((.*)\)" ==> t with
            | TwoPartsMatch(fnName, parts) ->
                let parts = parts |> toFunctionParams |> List.map parse
                Function(fnName, parts)
            | _ -> t |> SimpleValue
    parse text

let parseForCycle forAttr =
    match "(.+) in (.+)" ==> forAttr with
    | TwoPartsMatch(item, source) -> ForCycle(item, source |> parseTemplateValue) |> Some
    | _ -> None

let parseDiscriminatedUnion duAttr caseAttr =
    match "(.+?)\((.*)\)" ==> caseAttr with
    | TwoPartsMatch(caseName, parts) -> DiscriminatedUnion(duAttr, caseName, (parts |> toFunctionParams))
    | _ -> DiscriminatedUnion(duAttr, caseAttr, [])

let parseInclude srcAttr dataAttr =
    let localData = 
        dataAttr 
        |> split ';' 
        |> List.map (splitToTuple '=') 
        |> List.map (fun (k,v) -> k, parseTemplateValue(v))
    Include(srcAttr, localData)

let private getAttributeValue attr (node:HtmlNode) = Option.bind (fun (v:HtmlAttribute) -> v.Value() |> Some) (node.TryGetAttribute(attr)) 
let private (|ForCycle|_|) = getAttributeValue "fs-for"
let private (|IfCondition|_|) = getAttributeValue "fs-if"
let private (|DiscriminatedUnion|_|) (node:HtmlNode) = 
    match node.TryGetAttribute("fs-du"), node.TryGetAttribute("fs-case") with
    | Some(du), Some(case) -> (du.Value(), case.Value()) |> Some
    | _ -> None
let private (|Include|_|) (node:HtmlNode) = 
    match node.Name(), node.TryGetAttribute("fs-src"), node.TryGetAttribute("fs-data") with
    | "fs-include", Some(src), Some(data) -> (src.Value(), data.Value()) |> Some
    | _ -> None

let parseNode (node:HtmlNode) = 
    let someSuccess = Some >> Success
    match node with
    | ForCycle(attr) -> parseForCycle(attr) |> failForNone (CannotParseForCycle(attr)) >>= someSuccess
    | IfCondition(attr) -> attr |> parseTemplateValue |> IfCondition |> someSuccess
    | DiscriminatedUnion(du, case) -> parseDiscriminatedUnion du case |> someSuccess
    | Include(src, data) -> parseInclude src data |> someSuccess
    | _ -> None |> success

let parseTemplateText text = 
    let regex = new Regex("{{{(.*?)}}}", RegexOptions.IgnoreCase)
    [for m in regex.Matches(text) do yield m.Groups] 
    |> List.map (fun g -> g.[0].Value, (g.[1].Value |> clean))
    |> List.filter (fun x -> snd x <> "")