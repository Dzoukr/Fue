module internal Fue.ValueCompiler

open Core
open Data
open StringUtils
open System.Reflection
open Rop
open Reflection

type private Return =
    | Value of obj
    | Method of obj * MethodInfo

let private getObjectValue key value =
    match key, value with
    | "IsSome", null -> Return.Value(false) |> Some
    | "IsNone", null -> Return.Value(true) |> Some
    | k, v -> 
        match getValue k v with
        | Some(va) -> Return.Value(va) |> Some
        | _ -> None

let private getObjectMethod key value = Option.bind (fun mi -> Return.Method(value, mi) |> Some) (getMethod key value)
    
let private deepSearch data name props =
    let foldFn (acc:Result<Return>) (item:string) =
        match acc with
        | Success(Value(a)) ->
            let found = 
                [getObjectValue; getObjectMethod] 
                |> List.fold (fun ac fn -> match ac with | Some(acum) -> Some(acum) | None -> fn item a) None
            
            match found with
            | Some(value) -> value |> success
            | None -> NoMethodOrPropertyFound(a, item) |> fail
        | _ -> acc
    let structure = data |> get name |> Return.Value |> success
    props |> List.fold foldFn structure

let private search data key =
    match data |> tryGet key with
    | Some(value) -> value |> Return.Value |> success // direct match
    | None ->
        let name,props = key |> splitToFirstAndList '.'
        match props |> List.length with
        | 0 -> CannotExtractProperty(key) |> fail
        | _ -> deepSearch data name props

let private boxed obj =
    match obj |> isTuple with
    | true -> obj |> toObjectTuple
    | false -> obj

let private boxedArr arr = 
    match arr |> Array.length with
    | 0 -> [| () |> box |], [||], [| obj().GetType() |]
    | _ -> 
        let boxed = arr |> Array.map boxed
        let types = arr |> Array.map (fun x -> if x |> (isNull >> not) then x.GetType() else null)
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
                let funcParams, methodParams, types = pars |> List.map (comp >> Rop.extract) |> List.toArray |> boxedArr
                fnName |> search data <!> (fun v ->
                    match v with
                    | Value(func) ->
                        let invoke = func |> getFunctionInvoke types
                        invoke.Invoke(func, funcParams)
                    | Method(ob, mi) -> mi.Invoke(ob, methodParams)
                )
            | Literal(value) -> value |> success
            | NullCoalesce (left, right) ->
                match comp left with
                | Failure _ -> comp right
                | Success result -> Success result

            | Record(record) ->
                let compiledRecord =
                    record
                    |> Map.map (fun _ value -> (comp >> Rop.extract) value)

                compiledRecord
                |> box
                |> Success
                    
        comp value
    )
