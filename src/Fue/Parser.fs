module Fue.Parser

open Core
open System
open StringUtils
open System.Text.RegularExpressions
open HtmlAgilityPack
open Extensions
open FParsec

let forAttr = "fs-for"
let ifAttr = "fs-if"
let elseAttr = "fs-else"
let unionSourceAttr = "fs-du"
let unionCaseAttr = "fs-case"

let private (==>) regex value =
    let regex = new Regex(regex, RegexOptions.IgnoreCase ||| RegexOptions.Multiline)
    let m = regex.Match(value)
    m.Groups

let private splitByCurrying parseFn t = 
    
    let regex = new Regex(""""[^"]+"?|'[^']+'?|[^'"\s]+""", RegexOptions.IgnoreCase ||| RegexOptions.Singleline)
    let matches = [ for m in regex.Matches(t) do yield m.Groups.[0].Value ]
    let f,s = (matches |> List.head |> clean),(matches |> List.tail |> List.map clean)
    f, (s |> List.map parseFn)

let private (|TwoPartsMatch|_|) (groups:GroupCollection) =
    match groups.Count with
    | 3 ->
        let fnName = groups.[1].Value |> clean
        let parts = groups.[2].Value |> clean
        (fnName, parts) |> Some
    | _ -> None

let private (|OnePartMatch|_|) (groups:GroupCollection) =
    match groups.Count with
    | 2 -> groups.[1].Value |> clean |> Some
    | _ -> None

let private numberOrSimple value =
    match Int32.TryParse(value, Globalization.NumberStyles.Any, Globalization.NumberFormatInfo.InvariantInfo) with
    | true, value -> Literal(value)
    | _ ->
        match Decimal.TryParse(value, Globalization.NumberStyles.Any, Globalization.NumberFormatInfo.InvariantInfo) with 
        | true, value -> Literal(value)
        | _ ->
            match Double.TryParse(value, Globalization.NumberStyles.Any, Globalization.NumberFormatInfo.InvariantInfo) with 
            | true, value -> Literal(value)
            | _ -> SimpleValue(value)

/// Parse an escaped char
let escapedChar = 
    [ 
    // (stringToMatch, resultChar)
    ("\\\"",'\"')      // quote
    ("\\\\",'\\')      // reverse solidus 
    ("\\/",'/')        // solidus
    ("\\b",'\b')       // backspace
    ("\\f",'\f')       // formfeed
    ("\\n",'\n')       // newline
    ("\\r",'\r')       // cr
    ("\\t",'\t')       // tab
    ] 
    // convert each pair into a parser
    |> List.map (fun (toMatch,result) -> 
        pstring toMatch >>% result)
    // and combine them into one
    |> choice

/// Parse a unicode char
let unicodeChar = 

    // set up the "primitive" parsers        
    let backslash = pchar '\\'
    let uChar = pchar 'u'
    let hexdigit = anyOf (['0'..'9'] @ ['A'..'F'] @ ['a'..'f'])

    // convert the parser output (nested tuples)
    // to a char
    let convertToChar (((h1,h2),h3),h4) = 
        let str = sprintf "%c%c%c%c" h1 h2 h3 h4
        Int32.Parse(str,Globalization.NumberStyles.HexNumber) |> char

    // set up the main parser
    backslash  >>. uChar >>. hexdigit .>>. hexdigit .>>. hexdigit .>>. hexdigit
    |>> convertToChar 


/// Parse an unescaped char
let unescapedChar = 
    satisfyL (fun ch -> ch <> '\\' && ch <> '\"') "char"

let trippleQuotedString =
    let singleQuote =
        pchar '"'// .>> notFollowedByString "\"\""

    let trippleQuotes = pstring "\"\"\"" <?> "quote"
    let jchar = singleQuote <|> unescapedChar <|> escapedChar <|> unicodeChar <|> newline

    trippleQuotes >>. manyCharsTill jchar trippleQuotes <?> "triple quoted string"

let quote = pchar '"' <?> "quote"

let jchar = unescapedChar <|> escapedChar <|> unicodeChar

let singleQuotedString =

    // set up the main parser
    quote >>. manyChars jchar .>> quote 

let quotedString =
    trippleQuotedString <|> singleQuotedString .>> eof

let record =
    let leftBracket = pchar '{'
    let rightBracket = pchar '}'

    let recordLiteralEntry =
        let sep = (spaces >>. pchar '=' .>> spaces)
        let key = manyCharsTill asciiLetter sep
        let value = quote >>. manyCharsTill jchar quote .>> (optional (skipAnyOf [';'; '\n'; ' ']) >>. spaces ) |>> sprintf "\"%s\""

        key .>>. value

    let recordEntry = choice [
        recordLiteralEntry
    ]

    
    leftBracket >>. spaces >>. many recordEntry .>> rightBracket

let parseTemplateValue text =
    
    let pipedFn parseFn t =
        match "(.+)\|\>(.+)" ==> t with
        | TwoPartsMatch(parts, fnName) ->
            let f,p = fnName |> splitByCurrying parseFn
            Function(f, p @ [parts |> parseFn]) |> Some
        | _ -> None
    
    let bracketFn parseFn t =
        match """(.+?)\(((?:.|\r|\n)*)\)""" ==> t with
        | TwoPartsMatch(fnName, parts) ->
            let parts = parts |> splitToFunctionParams |> List.map parseFn
            Function(fnName, parts) |> Some
        | _ -> None
    
    let plainFn parseFn t =
        match """(.+?)\s+(.*)""" ==> (t |> clean) with
        | TwoPartsMatch(fnName, parts) ->
            let parts = parts |> split ' ' |> List.map parseFn
            Function(fnName, parts) |> Some
        | _ -> None
    
    let literalSQFn parseFn t = 
        match """^'([^']+)'$""" ==> (t |> clean) with
        | OnePartMatch(constant) -> Literal(constant) |> Some
        | _ -> None
    
    let literalDQFn parseFn t = 
        match """^"([^"]+)"$""" ==> (t |> clean) with
        | OnePartMatch(constant) -> Literal(constant) |> Some
        | _ -> None

    let literalTQFn parseFn t = 
        t
        |> clean
        |> run quotedString
        |> function
        | ParserResult.Failure (err, _, _) -> None
        | ParserResult.Success (constant, _, _) -> Literal(constant) |> Some

    let record (parseFn: string -> TemplateValue) t =
        t
        |> clean
        |> run record
        |> function
        | ParserResult.Failure (err, _, _) ->
            None
        | ParserResult.Success (recordEntries, _, _) ->
            recordEntries
            |> List.map(fun (key, value) ->
                (key, parseFn <| value)
            )
            |> Map.ofList
            |> Record
            |> Some
    
    let parseFns = [record;literalSQFn;literalDQFn;literalTQFn;pipedFn;bracketFn;plainFn;]

    let rec newParse t =
        let foldFn (acc:TemplateValue option) item =
            if acc.IsSome then acc
            else t |> item

        let chainResult =
            parseFns 
            |> List.map (fun x -> x newParse)
            |> List.fold foldFn None
        
        match chainResult with
        | Some v -> v
        | None -> t |> numberOrSimple

    newParse text

let parseForCycleAttribute forAttr =
    match "(.+) in (.+)" ==> forAttr with
    | TwoPartsMatch(item, source) -> ForCycle(item, source |> parseTemplateValue) |> Some
    | _ -> None

let parseUnionCaseAttribute caseAttr =
    match "(.+?)\((.*)\)" ==> caseAttr with
    | TwoPartsMatch(caseName, parts) -> caseName, (parts |> splitToFunctionParams)
    | _ -> caseAttr, []

let private getAttributeValue attr (node:HtmlNode) = Option.bind (fun (v:HtmlAttribute) -> v.Value |> Some) (node.TryGetAttribute(attr)) 
let private (|ForCycle|_|) = getAttributeValue forAttr
let private (|IfCondition|_|) = getAttributeValue ifAttr
let private (|ElseCondition|_|) = getAttributeValue elseAttr
let private (|DiscriminatedUnion|_|) (node:HtmlNode) = 
    match node.TryGetAttribute(unionSourceAttr), node.TryGetAttribute(unionCaseAttr) with
    | Some(du), Some(case) -> (du.Value, case.Value) |> Some
    | _ -> None

let parseNode (node:HtmlNode) = 
    match node with
    | ForCycle(attr) -> parseForCycleAttribute(attr)
    | IfCondition(attr) -> attr |> parseTemplateValue |> IfCondition |> Some
    | ElseCondition(_) -> ElseCondition |> Some
    | DiscriminatedUnion(du, case) -> 
        let c, extr = case |> parseUnionCaseAttribute 
        DiscriminatedUnion((du |> parseTemplateValue), c, extr) |> Some
    | _ -> None

let parseTextInterpolations text = 
    let regex = new Regex("{{{((.|\r|\n)*?)}}}", RegexOptions.IgnoreCase)
    [for m in regex.Matches(text) do yield m.Groups] 
    |> List.map (fun g -> g.[0].Value, (g.[1].Value |> clean))
    |> List.filter (fun x -> snd x <> "")