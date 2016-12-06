module Fue.Core

type TemplateValue =
    | SimpleValue of name:string
    | Function of name:string * parameters:TemplateValue list

type TemplateNode =
    | ForCycle of item:string * source:TemplateValue
    | IfCondition of source:TemplateValue
    | DiscriminatedUnion of union:string * case:string * extract:string list
    | Include of src:string * localData:((string * TemplateValue) list)





//let parseTemplateNode node =
    

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

