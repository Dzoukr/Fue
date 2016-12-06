module Fue.Compiler

open Core
open Data
open StringUtils
open Microsoft.FSharp.Reflection

let private isRecord obj = FSharpType.IsRecord(obj.GetType())
let private isTuple obj = FSharpType.IsTuple(obj.GetType())
let private isFunction obj = FSharpType.IsFunction(obj.GetType())
let private isClass obj = obj.GetType().IsClass

let private (|Record|_|) obj =
    match obj |> isRecord with
    | true -> Some obj
    | false -> None

let private (|Class|_|) obj =
    match obj |> isClass with
    | true -> Some obj
    | false -> None

let private getProperties = function
    | Record(rc) -> rc.GetType() |> FSharpType.GetRecordFields
    | Class(cls) -> cls.GetType().GetProperties()

let private getValue key value =
    getProperties value
    |> Array.filter (fun x -> x.Name = key) 
    |> Array.map (fun x -> x.GetValue(value)) 
    |> Array.head

let private toObjectTuple value =
    let props = FSharpValue.GetTupleFields(value)
    let types = [| for _ in 1..props.Length do yield obj().GetType() |]
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
                let v = getValue item <| fst acc
                match v |> isRecord with //! TODO: INVESTIGATE HERE!!!!
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
    | 0 -> [| () |> box |], [| obj().GetType() |]
    | _ -> (arr |> Array.map boxed), (arr |> Array.map (fun x -> x.GetType()))

let compile data value =
    let rec comp v =
        match v with
        | SimpleValue(valueName) -> valueName |> search data
        | Function(fnName, pars) -> 
            let parameters,types = pars |> List.map comp |> List.toArray |> boxedArr
            let func = fnName |> search data
            let invoke = if parameters.Length < 2 then func.GetType().GetMethod("Invoke") else func.GetType().GetMethod("Invoke", types)
            invoke.Invoke(func, parameters)
    comp value