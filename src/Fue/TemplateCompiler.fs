module Fue.TemplateCompiler

open Core
open Data
open StringUtils
open Rop

let private toStringValue = function
    | Success(v:obj) -> v |> string
    | Failure(errors) -> "CHYBA TODO"

let private toTemplateValues (original, value) = original, (value |> Parser.parseTemplateValue)
let private toCompiledValues data (original, templateValue) = original, (templateValue |> ValueCompiler.compile data)
let private toStringValues (original, result) = original, (result |> toStringValue)

let private convertToString data = 
    Parser.parseTextInterpolations
    >> List.map toTemplateValues
    >> List.map (toCompiledValues data)
    >> List.map toStringValues

let private replaceText (text:string) (replacements:(string * string) list) = 
    let foldFn (acc:string) (item:string * string) = acc.Replace(fst item, snd item)
    replacements |> List.fold foldFn text

/// Compiles templated string into final string
let compile data text = convertToString data text |> replaceText text