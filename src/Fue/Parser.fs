module Fue.Parser

open System
open Core
open System.Text.RegularExpressions

let private (==>) regex value =
    let regex = new Regex(regex, RegexOptions.IgnoreCase ||| RegexOptions.Singleline)
    regex.Match(value).Groups

let parseTemplateValue text =
    let clean (t:string) = t.Trim()
    let rec parse t =
        let groups = "(.+?)\((.*)\)" ==> t
        match groups.Count with
        | 3 -> 
            let fnName = groups.[1].Value |> clean
            let parts = 
                groups.[2].Value.Split([|','|], StringSplitOptions.RemoveEmptyEntries) 
                |> Array.map (clean >> parse) 
                |> Array.toList
            Function(fnName, parts)
        | _ -> t |> SimpleValue
    parse text

let parseForCycle text =
    let groups = "(.+) in (.+)" ==> text
    match groups.Count with
    | 3 -> ForCycle(groups.[1].Value, groups.[2].Value |> parseTemplateValue) |> Some
    | _ -> None