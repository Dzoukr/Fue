module internal Fue.StringUtils

open System

let clean (t:string) = t.Trim()
let split (separator:char) (s:string) = s.Split([|separator|], StringSplitOptions.RemoveEmptyEntries) |> Array.toList
let splitToTuple char = split char >> (fun ls -> ls.[0], ls.[1])
let splitToFunctionParams = split ',' >> List.map clean
let splitToFirstAndList (separator:char) (s:string) = 
    let parts = s |> split separator |> List.map clean
    match parts.Length with
    | 1 -> s, []
    | _ -> parts.[0], [ for i in 1..parts.Length - 1 do yield parts.[i] ]