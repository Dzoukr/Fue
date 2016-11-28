module Fue.Core

open System
open FSharp.Data
open System.Text.RegularExpressions
open Rop

type TemplateValue =
    | SimpleValue of name:string
    | Function of name:string * parameters:TemplateValue list

type TemplateNode =
    | ForCycle of item:string * source:TemplateValue
    | IfCondition of source:TemplateValue
    | DiscriminatedUnion of union:string * case:string * extract:string list option
    | Include of src:string * closures:((string * string) list)

let (==>) regex value =
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

//let forCycleNode value =
//    let groups = "(.+) in (.+)" ==> value
//    match groups.Count with
//    | 3 -> ForCycle(groups.[1].Value, groups.[2].Value) |> success
//    | _ -> ForCycleHasInvalidParams(value) |> fail
//
//let ifNode value =
//    let groups = "(.+?)\((.*)\)" ==> value
//    let x = groups.[0]
//    match groups.Count with
//    | 2 -> 
//        let func = groups.[1].Value
//        let pars = groups.[2].Value
//
//    | 0 -> IfCondition(value, None)



//let (|ForCycle|_|) (node:HtmlNode) = 
//    match node.TryGetAttribute("fs-for") with
//    | Some(attr) -> attr.Value() |> forCycleNode |> Some
//    | None -> None
//
//
//let (|IfCondition|_|) (node:HtmlNode) = node.TryGetAttribute("fs-if")
//let (|DiscriminatedUnion|_|) (node:HtmlNode) = 
//    match node.TryGetAttribute("fs-du"), node.TryGetAttribute("fs-case") with
//    | Some(du), Some(case) -> (du, case) |> Some
//    | _ -> None
//let (|Include|_|) (node:HtmlNode) = 
//    match node.Name(), node.TryGetAttribute("fs-src"), node.TryGetAttribute("fs-data") with
//    | "fs-include", Some(src), Some(data) -> (src, data) |> Some
//    | _ -> None
//
//
//    


// data
let init = Map.empty
let add (key:string) (value:obj) (map:Map<string, obj>) = map.Add(key, value)
let get (key:string) (map:Map<string, obj>) = map.TryFind key

let x = fun y -> y + 10

init |> add "tuple" (1,2) |> add "record" "abc" |> add "function" x

