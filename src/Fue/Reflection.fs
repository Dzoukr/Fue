module Fue.Reflection

open Microsoft.FSharp.Reflection
open System.Reflection

let private getSafeValue value (info:PropertyInfo) =
    let pars = info.GetIndexParameters()
    match pars.Length with
    | 1 -> info.GetValue(value, [|value|])
    | _ -> info.GetValue(value)

let getValue key value =
    value.GetType().GetProperties()
    |> Array.filter (fun x -> x.Name = key) 
    |> Array.map (getSafeValue value)
    |> Array.tryHead

let getMethod key value =
    value.GetType().GetMethods()
    |> Array.filter (fun x -> x.Name = key)
    |> Array.tryHead

let isTuple obj = FSharpType.IsTuple(obj.GetType())

let toObjectTuple value =
    let props = FSharpValue.GetTupleFields(value)
    let types = [| for _ in 1..props.Length do yield obj().GetType() |]
    let typ = FSharpType.MakeTupleType(types)
    FSharpValue.MakeTuple(props, typ)

let getFunctionInvoke (types:System.Type []) func =
    if types.Length > 1 then
        func.GetType().GetMethod("Invoke", types)
    else
        func.GetType().GetMethod("Invoke")