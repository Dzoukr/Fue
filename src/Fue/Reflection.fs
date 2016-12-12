module Fue.Reflection

open Microsoft.FSharp.Reflection

let getValue key value =
    value.GetType().GetProperties()
    |> Array.filter (fun x -> x.Name = key) 
    |> Array.map (fun x -> x.GetValue(value)) 
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