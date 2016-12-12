module Fue.ValueCompiler

open Core
open Data
open StringUtils
open Microsoft.FSharp.Reflection
open System.Reflection
open Rop
open Reflection

type private Return =
    | Value of obj
    | Method of obj * MethodInfo

let private search data key =
    match data |> tryGet key with
    | Some(value) -> value |> Return.Value |> success // direct match
    | None ->
        let name,props = key |> splitToFirstAndList '.'
        match props |> List.length with
        | 0 -> CannotExtractProperty(key) |> fail
        | _ -> 
            let foldFn (acc:Result<Return>) (item:string) =
                match acc with
                | Success(Value(a)) ->
                    match a |> getValue item, a |> getMethod item with
                    | Some(value),_ -> value |> Return.Value |> success
                    | _,Some(mi) -> Return.Method(a, mi) |> success
                    | _ -> NoMethodOrPropertyFound(a, item) |> fail
                | _ -> acc
            let structure = data |> get name |> Return.Value |> success
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

let private extractSimpleValue value = 
    match value with
    | Success(Value(obj)) -> obj |> success
    | Success(Method(obj,_)) -> ValueExpectedToBeSimple(obj) |> fail
    | Failure(f) -> f |> Failure

/// Compiles template value into obj
let compile data value =
    catch(fun _ ->
        let rec comp v =
            match v with
            | SimpleValue(valueName) -> valueName |> search data |> extractSimpleValue
            | Function(fnName, pars) -> 
                let funcParams, methodParams, types = pars |> List.map comp |> List.map Rop.extract |> List.toArray |> boxedArr
                fnName |> search data >>=> (fun v ->
                    match v with
                    | Value(func) -> 
                        let invoke = func |> getFunctionInvoke types
                        invoke.Invoke(func, funcParams)
                    | Method(ob, mi) -> mi.Invoke(ob, methodParams)
                )
        comp value
    )