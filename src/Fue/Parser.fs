module Fue.Parser

open System
open Core
open FSharp.Data
open System.Text.RegularExpressions

let private (==>) regex value =
    let regex = new Regex(regex, RegexOptions.IgnoreCase ||| RegexOptions.Singleline)
    regex.Match(value).Groups

let private clean (t:string) = t.Trim()
let private split (separator:char) (s:string) = s.Split([|separator|], StringSplitOptions.RemoveEmptyEntries) |> Array.toList
let private splitToTuple char = split char >> (fun ls -> ls.[0], ls.[1])

let private (|ThreePart|_|) (groups:GroupCollection) =
    match groups.Count with
    | 3 ->
        let fnName = groups.[1].Value |> clean
        let parts = groups.[2].Value |> split ',' |> List.map clean
        (fnName, parts) |> Some
    | _ -> None

let parseTemplateValue text =
    let rec parse t =
        match "(.+?)\((.*)\)" ==> t with
        | ThreePart(fnName, parts) ->
            let parts = parts |> List.map parse
            Function(fnName, parts)
        | _ -> t |> SimpleValue
    parse text

let parseForCycle text =
    let groups = "(.+) in (.+)" ==> text
    match groups.Count with
    | 3 -> ForCycle(groups.[1].Value, groups.[2].Value |> parseTemplateValue) |> Some
    | _ -> None

let parseDUExtract text =
    match "(.+?)\((.*)\)" ==> text with
    | ThreePart(caseName, parts) -> caseName, parts
    | _ -> text, []

let parseIncludeData text =
    text 
    |> split ';' 
    |> List.map (splitToTuple '=') 
    |> List.map (fun (k,v) -> k, parseTemplateValue(v))
    
    