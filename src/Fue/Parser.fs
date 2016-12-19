module Fue.Parser

open System
open Core
open StringUtils
open System.Text.RegularExpressions
open HtmlAgilityPack
open Rop
open Extensions

let forAttr = "fs-for"
let ifAttr = "fs-if"
let ifNotAttr = "fs-if-not"
let unionSourceAttr = "fs-du"
let unionCaseAttr = "fs-case"

let private (==>) regex value =
    let regex = new Regex(regex, RegexOptions.IgnoreCase ||| RegexOptions.Singleline)
    regex.Match(value).Groups

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
                let parts = parts |> splitToFunctionParams |> List.map parse
                Function(fnName, parts)
            | _ -> t |> SimpleValue
    parse text

let parseForCycleAttribute forAttr =
    match "(.+) in (.+)" ==> forAttr with
    | TwoPartsMatch(item, source) -> ForCycle(item, source |> parseTemplateValue) |> Some
    | _ -> None

let parseUnionCaseAttribute caseAttr =
    match "(.+?)\((.*)\)" ==> caseAttr with
    | TwoPartsMatch(caseName, parts) -> caseName, (parts |> splitToFunctionParams)
    | _ -> caseAttr, []

let private getAttributeValue attr (node:HtmlNode) = Option.bind (fun (v:HtmlAttribute) -> v.Value |> Some) (node.TryGetAttribute(attr)) 
let private (|ForCycle|_|) = getAttributeValue forAttr
let private (|IfCondition|_|) = getAttributeValue ifAttr
let private (|IfNotCondition|_|) = getAttributeValue ifNotAttr
let private (|DiscriminatedUnion|_|) (node:HtmlNode) = 
    match node.TryGetAttribute(unionSourceAttr), node.TryGetAttribute(unionCaseAttr) with
    | Some(du), Some(case) -> (du.Value, case.Value) |> Some
    | _ -> None

let parseNode (node:HtmlNode) = 
    let someSuccess = Some >> Success
    match node with
    | ForCycle(attr) -> parseForCycleAttribute(attr) |> failForNone (CannotParseForCycle(attr)) >>= someSuccess
    | IfCondition(attr) -> attr |> parseTemplateValue |> IfCondition |> someSuccess
    | IfNotCondition(attr) -> attr |> parseTemplateValue |> IfNotCondition |> someSuccess
    | DiscriminatedUnion(du, case) -> 
        let c, extr = case |> parseUnionCaseAttribute 
        DiscriminatedUnion((du |> parseTemplateValue), c, extr) |> someSuccess
    | _ -> None |> success

let parseTextInterpolations text = 
    let regex = new Regex("{{{(.*?)}}}", RegexOptions.IgnoreCase)
    [for m in regex.Matches(text) do yield m.Groups] 
    |> List.map (fun g -> g.[0].Value, (g.[1].Value |> clean))
    |> List.filter (fun x -> snd x <> "")