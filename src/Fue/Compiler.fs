module Fue.Compiler

open Core
open Data
open StringUtils
open Microsoft.FSharp.Reflection
open System.Reflection

let private isTuple obj = FSharpType.IsTuple(obj.GetType())

type private Return =
    | NotFound
    | Value of obj
    | Method of obj * MethodInfo

let private getProperties ob = ob.GetType().GetProperties() 
let private getMethods ob = ob.GetType().GetMethods()

let private getValue key value =
    getProperties value
    |> Array.filter (fun x -> x.Name = key) 
    |> Array.map (fun x -> x.GetValue(value)) 
    |> Array.tryHead

let private getMethod key value =
    getMethods value
    |> Array.filter (fun x -> x.Name = key)
    |> Array.tryHead

let private toObjectTuple value =
    let props = FSharpValue.GetTupleFields(value)
    let types = [| for _ in 1..props.Length do yield obj().GetType() |]
    let typ = FSharpType.MakeTupleType(types)
    FSharpValue.MakeTuple(props, typ)

let private search data key =
    match data |> tryGet key with
    | Some(value) -> value |> Return.Value // direct match
    | None ->
        let name,props = key |> splitToFirstAndList '.'
        match props |> List.length with
        | 0 -> Return.NotFound
        | _ -> 
            let foldFn (acc:Return) (item:string) =
                match acc with
                | Value(a) ->
                    match a |> getValue item, a |> getMethod item with
                    | Some(value),_ -> value |> Return.Value
                    | _,Some(mi) -> Return.Method(a, mi)
                    | _ -> Return.NotFound
                | _ -> acc
            let structure = data |> get name |> Return.Value
            props |> List.fold foldFn structure

let private boxed obj =
    match obj |> isTuple with
    | true -> obj |> toObjectTuple
    | false -> obj

let private boxedArr arr = 
    match arr |> Array.length with
    | 0 -> [| () |> box |], [||], [| obj().GetType() |]
    | _ -> 
        let boxed = arr |> Array.map boxed
        let types = arr |> Array.map (fun x -> x.GetType())
        boxed, boxed, types

let private getFunctionInvoke (types:System.Type []) func =
    if types.Length > 1 then
        func.GetType().GetMethod("Invoke", types)
    else
        func.GetType().GetMethod("Invoke")

let private extract = function
    | Value(obj) -> obj
    | _ -> "NOT FOUND TODO" |> box

let compile data value =
    let rec comp v =
        match v with
        | SimpleValue(valueName) -> valueName |> search data |> extract
        | Function(fnName, pars) -> 
            let funcParams, methodParams, types = pars |> List.map comp |> List.toArray |> boxedArr
            match fnName |> search data with
            | Value(func) -> 
                let invoke = func |> getFunctionInvoke types
                invoke.Invoke(func, funcParams)
            | Method(ob, mi) -> mi.Invoke(ob, methodParams)
            | NotFound -> failwith "Not implemented yet"
    comp value