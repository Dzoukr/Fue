module Fue.Compiler

open Core
open Data
open StringUtils
open Microsoft.FSharp.Reflection
open System.Reflection

let private isRecord obj = FSharpType.IsRecord(obj.GetType())
let private isTuple obj = FSharpType.IsTuple(obj.GetType())
let private isFunction obj = FSharpType.IsFunction(obj.GetType())
let private isClass obj = obj.GetType().IsClass
let private isValue obj = obj.GetType().IsValueType

type private Return =
    | Value of obj
    | Method of obj * System.Reflection.MethodInfo

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

let private getMethods = function
    | Record(ob) | Class(ob) -> 
        let methods = ob.GetType().GetMethods()
        let members = ob.GetType().GetMembers()
        methods

let private getValue key value =
    getProperties value
    |> Array.filter (fun x -> x.Name = key) 
    |> Array.map (fun x -> x.GetValue(value)) 
    |> Array.tryHead

let private getMethod key value =
    getMethods value
    |> Array.filter (fun x -> x.Name = key)
    |> Array.head

let private toObjectTuple value =
    let props = FSharpValue.GetTupleFields(value)
    let types = [| for _ in 1..props.Length do yield obj().GetType() |]
    let typ = FSharpType.MakeTupleType(types)
    FSharpValue.MakeTuple(props, typ)

let private search data key =
    match data |> Data.tryGet key with
    | Some(value) -> value |> Return.Value // direct match
    | None ->
        let name,props = key |> splitToFirstAndList '.'
        match props |> List.length with
        | 0 -> "NOT FOUND TODO" |> box |> Return.Value
        | _ -> 
            let foldFn (acc:Return) (item:string) =
                match acc with
                | Value(a) ->
                    match a |> getValue item with
                    | Some(value) -> value |> Return.Value
                    | None -> 
                        let mi = a |> getMethod item
                        Return.Method(a, mi)
                | Method(_) -> acc

            let structure = data |> get name |> Return.Value
            props |> List.fold foldFn structure
            //props |> List.fold (fun a i -> a |> getValue i) structure

let private boxed obj =
    match obj |> isTuple with
    | true -> obj |> toObjectTuple
    | false -> obj

let private boxedArr arr = 
    match arr |> Array.length with
    | 0 -> [| () |> box |], [| obj().GetType() |]
    | _ -> (arr |> Array.map boxed), (arr |> Array.map (fun x -> x.GetType()))

let private extract = function
    | Value(obj) -> obj
    | Method(_) -> "NOT FOUND" |> box

let compile data value =
    let rec comp v =
        match v with
        | SimpleValue(valueName) -> valueName |> search data |> extract
        | Function(fnName, pars) -> 
            let parameters,types = pars |> List.map comp |> List.toArray |> boxedArr
            match fnName |> search data with
            | Value(func) ->
                let invoke = if parameters.Length < 2 then func.GetType().GetMethod("Invoke") else func.GetType().GetMethod("Invoke", types)
                invoke.Invoke(func, parameters)
            | Method(ob, mi) -> mi.Invoke(ob, parameters)
    comp value