module Fue.Compiler

open Core
open Data
open StringUtils
open Microsoft.FSharp.Reflection

let private isRecord obj = FSharpType.IsRecord(obj.GetType())
let private isFunction obj = FSharpType.IsFunction(obj.GetType())
let private getRecordValue key obj =
    FSharpType.GetRecordFields(obj.GetType()) 
    |> Array.filter (fun x -> x.Name = key) 
    |> Array.map (fun x -> x.GetValue(obj)) 
    |> Array.head


let private compileSimpleValue data key =
    match data |> Data.tryGet key with
    | Some(value) -> value // direct match
    | None ->
        let recName,props = key |> splitToFirstAndList '.'
        match props |> List.length with
        | 0 -> "NOT FOUND TODO" |> box
        | _ -> 
            let foldFn (acc:obj * obj) (item:string) =
                let v = getRecordValue item <| fst acc
                match v |> isRecord with
                | true -> v, (snd acc)
                | false -> (fst acc), v
            let record = data |> get recName
            props |> List.fold foldFn (record, obj()) |> snd

let compile data value =
    let parsOrUnit arr = 
        match arr |> Array.length with
        | 0 -> [| () |> box |]
        | _ -> arr
    
    let rec comp v =
        match v with
        | SimpleValue(valueName) -> valueName |> compileSimpleValue data
        | Function(fnName, pars) -> 
            let p = pars |> List.map comp |> List.toArray |> parsOrUnit
            let fn = data |> get fnName
            let invoke = fn.GetType().GetMethod("Invoke")
            invoke.Invoke(fn, p)

            
    comp value