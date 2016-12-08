module Fue.Parser

open System
open Core
open StringUtils
open System.Text.RegularExpressions

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