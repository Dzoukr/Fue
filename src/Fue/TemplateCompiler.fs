module internal Fue.TemplateCompiler

open Core
open Rop

let private toTemplateValues (original, value) = original, (value |> Parser.parseTemplateValue)
let private toCompiledValues data (original, templateValue) = original, (templateValue |> ValueCompiler.compile data)

let private convertToCompiledValues data = 
    Parser.parseTextInterpolations
    >> List.map toTemplateValues
    >> List.map (toCompiledValues data)

let private escape (v:obj) =
    v |> string |> System.Web.HttpUtility.HtmlEncode

let private replaceText mapFn (text:string) (replacements:(string * Result<obj>) list) = 
    let foldFn (acc:Result<string>) (item:string * Result<obj>) = 
        match acc, item with
        | Success(a), (key, Success(value)) -> a.Replace(key, mapFn value) |> success
        | Failure(f1), (_, Failure(f2)) -> f1 @ f2 |> Failure
        | res, _ -> res
    replacements |> List.fold foldFn (Success(text))

/// Compiles templated string into final string
let compile data text = convertToCompiledValues data text |> replaceText string text
/// Compiles templated string into final string with escaping dangerous chars
let compileSafe data text = convertToCompiledValues data text |> replaceText escape text