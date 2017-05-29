module Fue.Parser

open Core
open System
open StringUtils
open System.Text.RegularExpressions
open HtmlAgilityPack
open Extensions

let forAttr = "fs-for"
let ifAttr = "fs-if"
let elseAttr = "fs-else"
let unionSourceAttr = "fs-du"
let unionCaseAttr = "fs-case"

let private (==>) regex value =
    let regex = new Regex(regex, RegexOptions.IgnoreCase ||| RegexOptions.Singleline)
    regex.Match(value).Groups

let private splitByCurrying parseFn t = 
    
    let regex = new Regex(""""[^"]+"?|'[^']+'?|[^'"\s]+""", RegexOptions.IgnoreCase ||| RegexOptions.Singleline)
    let matches = [ for m in regex.Matches(t) do yield m.Groups.[0].Value ]
    let f,s = (matches |> List.head |> clean),(matches |> List.tail |> List.map clean)
    f, (s |> List.map parseFn)

let private (|TwoPartsMatch|_|) (groups:GroupCollection) =
    match groups.Count with
    | 3 ->
        let fnName = groups.[1].Value |> clean
        let parts = groups.[2].Value |> clean
        (fnName, parts) |> Some
    | _ -> None

let private (|OnePartMatch|_|) (groups:GroupCollection) =
    match groups.Count with
    | 2 -> groups.[1].Value |> clean |> Some
    | _ -> None

let private numberOrSimple value =
    match Int32.TryParse(value, Globalization.NumberStyles.Any, Globalization.NumberFormatInfo.InvariantInfo) with
    | true, value -> Literal(value)
    | _ ->
        match Decimal.TryParse(value, Globalization.NumberStyles.Any, Globalization.NumberFormatInfo.InvariantInfo) with 
        | true, value -> Literal(value)
        | _ ->
            match Double.TryParse(value, Globalization.NumberStyles.Any, Globalization.NumberFormatInfo.InvariantInfo) with 
            | true, value -> Literal(value)
            | _ -> SimpleValue(value)

let parseTemplateValue text =
    
    let pipedFn parseFn t =
        match "(.+)\|\>(.+)" ==> t with
        | TwoPartsMatch(parts, fnName) ->
            let f,p = fnName |> splitByCurrying parseFn
            Function(f, p @ [parts |> parseFn]) |> Some
        | _ -> None
    
    let bracketFn parseFn t =
        match """(.+?)\((.*)\)""" ==> t with
        | TwoPartsMatch(fnName, parts) ->
            let parts = parts |> splitToFunctionParams |> List.map parseFn
            Function(fnName, parts) |> Some
        | _ -> None
    
    let plainFn parseFn t =
        match """(.+?)\s+(.*)""" ==> (t |> clean) with
        | TwoPartsMatch(fnName, parts) ->
            let parts = parts |> split ' ' |> List.map parseFn
            Function(fnName, parts) |> Some
        | _ -> None
    
    let literalSQFn parseFn t = 
        match """^'([^']+)'$""" ==> (t |> clean) with
        | OnePartMatch(constant) -> Literal(constant) |> Some
        | _ -> None
    
    let literalDQFn parseFn t = 
        match """^"([^"]+)"$""" ==> (t |> clean) with
        | OnePartMatch(constant) -> Literal(constant) |> Some
        | _ -> None
    
    let parseFns = [literalSQFn;literalDQFn;pipedFn;bracketFn;plainFn;]

    let rec newParse t =
        let foldFn (acc:TemplateValue option) item =
            if acc.IsSome then acc
            else t |> item

        let chainResult =
            parseFns 
            |> List.map (fun x -> x newParse)
            |> List.fold foldFn None
        
        match chainResult with
        | Some v -> v
        | None -> t |> numberOrSimple

    newParse text

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
let private (|ElseCondition|_|) = getAttributeValue elseAttr
let private (|DiscriminatedUnion|_|) (node:HtmlNode) = 
    match node.TryGetAttribute(unionSourceAttr), node.TryGetAttribute(unionCaseAttr) with
    | Some(du), Some(case) -> (du.Value, case.Value) |> Some
    | _ -> None

let parseNode (node:HtmlNode) = 
    match node with
    | ForCycle(attr) -> parseForCycleAttribute(attr)
    | IfCondition(attr) -> attr |> parseTemplateValue |> IfCondition |> Some
    | ElseCondition(_) -> ElseCondition |> Some
    | DiscriminatedUnion(du, case) -> 
        let c, extr = case |> parseUnionCaseAttribute 
        DiscriminatedUnion((du |> parseTemplateValue), c, extr) |> Some
    | _ -> None

let parseTextInterpolations text = 
    let regex = new Regex("{{{(.*?)}}}", RegexOptions.IgnoreCase)
    [for m in regex.Matches(text) do yield m.Groups] 
    |> List.map (fun g -> g.[0].Value, (g.[1].Value |> clean))
    |> List.filter (fun x -> snd x <> "")