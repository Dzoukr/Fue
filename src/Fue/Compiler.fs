module Fue.Compiler

open Core
open Data
open StringUtils
open Microsoft.FSharp.Reflection

let private isRecord obj = FSharpType.IsRecord(obj.GetType())
let private isTuple obj = FSharpType.IsTuple(obj.GetType())
let private isFunction obj = FSharpType.IsFunction(obj.GetType())
let private getRecordValue key obj =
    FSharpType.GetRecordFields(obj.GetType()) 
    |> Array.filter (fun x -> x.Name = key) 
    |> Array.map (fun x -> x.GetValue(obj)) 
    |> Array.head

let private toObjectTuple value =
    let props = FSharpValue.GetTupleFields(value)
    let types = [| for i in 1..props.Length do yield obj().GetType() |]
    let typ = FSharpType.MakeTupleType(types)
    FSharpValue.MakeTuple(props, typ)

let private search data key =
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

let private boxed obj =
    match obj |> isTuple with
    | true -> obj |> toObjectTuple
    | false -> obj

let private boxedArr arr = 
    match arr |> Array.length with
    | 0 -> [| () |> box |]
    | _ -> arr |> Array.map boxed

let compile data value =
    let rec comp v =
        match v with
        | SimpleValue(valueName) -> valueName |> search data
        | Function(fnName, pars) -> 
            let p = pars |> List.map comp |> List.toArray |> boxedArr
            let fn = fnName |> search data
            let invoke = fn.GetType().GetMethod("Invoke")
            invoke.Invoke(fn, p)
    comp value