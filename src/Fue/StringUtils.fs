module internal Fue.StringUtils

open System
open System.Text.RegularExpressions

let isWhiteSpace (t:string) = String.IsNullOrWhiteSpace(t)
let clean (t:string) = t.Trim()
let split (separator:char) (s:string) = s.Split([|separator|], StringSplitOptions.RemoveEmptyEntries) |> Array.toList
let splitAsFunction (separator:char) (s:string) =
    let pattern = sprintf "%c(?![^(]*\))" separator
    Regex.Split(s, pattern) |> Array.toList |> List.filter (isWhiteSpace >> not)

let splitToFunctionParams = splitAsFunction ',' >> List.map clean
let splitToFirstAndList (separator:char) (s:string) = 
    let parts = s |> split separator |> List.map clean
    match parts.Length with
    | 1 -> s, []
    | _ -> parts.[0], [ for i in 1..parts.Length - 1 do yield parts.[i] ]