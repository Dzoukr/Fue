module Fue.Compiler

open Core
open Data
open StringUtils
open Microsoft.FSharp.Reflection

let private isRecord obj = FSharpType.IsRecord(obj.GetType())
let private getRecordValue key obj =
    FSharpType.GetRecordFields(obj.GetType()) 
    |> Array.filter (fun x -> x.Name = key) 
    |> Array.map (fun x -> x.GetValue(obj)) 
    |> Array.head


let private compileSimpleValue data key =
    match data |> Data.tryGet key with
    | Some(value) -> value |> string // direct match
    | None ->
        let recName,props = key |> splitToFirstAndList '.'
        match props |> List.length with
        | 0 -> "NOT FOUND TODO"
        | _ -> 
            let foldFn (acc:obj * string) (item:string) =
                let v = getRecordValue item <| fst acc
                match v |> isRecord with
                | true -> v, (snd acc)
                | false -> (fst acc), (v |> string)
            
            let record = data |> get recName // |> getRecordValue (props.Head) |> string
            let res = props |> List.fold foldFn (record, "")
            snd res

let compile data value =
    match value with
    | SimpleValue(valueName) -> valueName |> compileSimpleValue data
    | Function(fnName, pars) -> 
        let fn = data |> get fnName
        fnName