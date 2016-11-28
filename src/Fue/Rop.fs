module Fue.Rop

open System

type Error =
    // General
    | GeneralError of Exception
    | ForCycleHasInvalidParams of value:string
    | IfConditionHasInvalidParams of value:string

let explain = function
    // General
    | GeneralError(ex) -> sprintf "Unhandled exception \"%s\" occured. StackTrace: %s" ex.Message ex.StackTrace

type Result<'s> =
    | Success of 's
    | Failure of Error list

let success item = Success(item)
let fail error = Failure([error])
let failForNone error = function
    | Some(res) -> res |> success
    | None -> error |> fail

let extract = function
    | Success(item) -> item
    | Failure(_) -> failwith "No value to extract from Failure object"

let tryExtract = function
    | Success(item) -> Some item
    | Failure(_) -> None

let catch func =
    try
        func()
    with :? System.Exception as ex -> GeneralError(ex) |> fail

let bind f result =
    match result with
    | Success s -> f s
    | Failure f -> Failure f

let bind2 f x y = 
    match x, y with
    | Success xR, Success yR -> f xR yR
    | Failure eR, Failure eL -> Failure <| eR @ eL
    | Failure e, _ | _, Failure e -> Failure e

let bind3 f x y z =
    match x, y, z with
    | Success xR, Success yR, Success zR -> f xR yR zR
    | Failure xF, Failure yF, Failure zF -> Failure <| xF @ yF @ zF
    | Failure xF, Failure yF, _ -> Failure <| xF @ yF
    | Failure xF, _, Failure zF -> Failure <| xF @ zF
    | _, Failure yF, Failure zF -> Failure <| yF @ zF
    | Failure xF, _, _ -> Failure xF
    | _, Failure yF, _ -> Failure yF
    | _, _, Failure zF -> Failure zF

let (>>=) result f = bind f result
let (>>=>) result f = bind (f >> Success) result
let (>=>>) f result = bind result ( f |> Success)