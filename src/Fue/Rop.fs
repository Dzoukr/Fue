module Fue.Rop

open System

type Error =
    // General
    | GeneralError of Exception
    // Compiler
    | CannotExtractProperty of key:string
    | NoMethodOrPropertyFound of source:obj * name:string
    | ValueExpectedToBeSimple of source:obj
    // Parser
    | CannotParseForCycle of string
    // Node compiler
    | ValueExpectedToBeBoolean of value:obj
    | ValueExpectedToBeIterable of value:obj
    | ListOfDUExtractionHasDifferentLength of case:string * extractions:int * cases:int

let explain = function
    // General
    | GeneralError(ex) -> sprintf "Unhandled exception \"%s\" occured. StackTrace: %s" ex.Message ex.StackTrace
    // Compiler
    | CannotExtractProperty(key) -> sprintf "Cannot extract property after \".\" symbol from \"%s\"" key
    | NoMethodOrPropertyFound(source, name) -> sprintf "No property or method named \"%s\" found for \"%A\"" name source
    | ValueExpectedToBeSimple(source) -> sprintf "Value \"%A\" is expected to be simple value, not method" source
    // Parser
    | CannotParseForCycle(value) -> sprintf "Cannot parse For-cycle logic from value \"%s\"" value
    // Node compiler
    | ValueExpectedToBeBoolean(value) -> sprintf "Value \"%A\" is expected to be boolean" value
    | ValueExpectedToBeIterable(value) -> sprintf "Value \"%A\" is expected to be iterable" value
    | ListOfDUExtractionHasDifferentLength(case, extr, vals) -> sprintf "Case %s has %i values associated, but you want to extract %i values" case extr vals

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
    | Failure(errors) -> 
        errors 
        |> List.map explain 
        |> List.fold (fun a i -> a + ", " + i) ""
        |> sprintf "Success expected, but result state is Failure with errors: %s"
        |> failwith

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

let fold results =
    let foldFn acc item =
        match acc, item with
        | Success(res), Success(list) -> res @ list |> Success
        | Success(_), Failure(errors) -> errors |> Failure
        | Failure(errs), Failure(newErrs) -> errs @ newErrs |> Failure
        | Failure(errs), Success(_) -> errs |> Failure
    results |> List.fold foldFn (Success [])

let (>>=) result f = bind f result
let (>>=>) result f = bind (f >> Success) result
let (>=>>) f result = bind result ( f |> Success)
