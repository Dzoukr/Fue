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

    

let (<!>) (p: Parser<_,_>) label : Parser<_,_> =
    fun stream ->
        printfn "%A: Entering %s" stream.Position label
        let reply = p stream
        
        match reply.Status with
        | ReplyStatus.Ok ->
            printfn "%A: Leaving %s (%A)" stream.Position label reply.Status
        | ReplyStatus.Error ->
            printfn "%A: Leaving %s (%A)" stream.Position label reply.Status
            printfn "%A" (ErrorMessageList.ToSortedArray reply.Error |> Array.map (fun x -> x.ToString()))
        | ReplyStatus.FatalError ->
            printfn "%A: Leaving %s (%A)" stream.Position label reply.Status
            printfn "%A" (ErrorMessageList.ToSortedArray reply.Error)

        reply

/// Parse an unescaped char
let unescapedChar = 
    satisfyL (fun ch -> ch <> '\\' && ch <> '\"') "char"

let singleQuote = pchar ''' <?> "quote"
let quote = pchar '"' <?> "quote"

let jchar = unescapedChar <|> escapedChar <|> unicodeChar

let identifier = many1Satisfy2 isLetter (fun c -> isLetter c || isDigit c || c = '.') <?> "identifier"

let singleQuoteLiteral =
    singleQuote >>. manyCharsTill jchar singleQuote
    <?> "single quoted string"
    |>> fun literal -> Literal(literal)

let doubleQuoteLiteral =
    quote >>. manyCharsTill jchar quote
    <?> "quoted string"
    |>> fun literal -> Literal(literal)

let trippleQuoteLiteral =
    let singleQuote =
        pchar '"'// .>> notFollowedByString "\"\""

    let trippleQuotes = pstring "\"\"\"" <?> "quote"
    let jchar = choice [
        newline
        singleQuote
        unescapedChar
        escapedChar
        unicodeChar
    ]

    trippleQuotes >>. manyCharsTill jchar trippleQuotes
    <?> "triple quoted string"
    |>> fun literal -> Literal(literal)

let integer =
    many1Satisfy isDigit
    <?> "integer"
    |>> fun value ->
        match Int32.TryParse(value, Globalization.NumberStyles.Any, Globalization.NumberFormatInfo.InvariantInfo) with
        | true, value -> Literal(value)
        | _ -> SimpleValue value

let decimal =
    many1Satisfy (fun c -> isDigit c || c = '.')
    <?> "integer"
    |>> fun value ->
        match Decimal.TryParse(value, Globalization.NumberStyles.Any, Globalization.NumberFormatInfo.InvariantInfo) with
        | true, value -> Literal(value)
        | _ -> 
            match Double.TryParse(value, Globalization.NumberStyles.Any, Globalization.NumberFormatInfo.InvariantInfo) with 
            | true, value -> Literal(value)
            | _ -> SimpleValue(value)

let number =
    choice [
        attempt integer
        attempt decimal
    ]
    <?> "number"
    <!> "number"
 
let literal =
    choice [
        trippleQuoteLiteral
        doubleQuoteLiteral
        singleQuoteLiteral
    ]
    <?> "literal"
    <!> "literal"

let expression, expressionImpl = createParserForwardedToRef()

let record =
    let leftBracket = pchar '{'
    let rightBracket = pchar '}'

    let recordEntry =
        let sep = (spaces >>. pchar '=' .>> spaces)
        let key = identifier .>> sep

        key .>>. expression

    let newlineOrSemiColon =
        manySatisfy (function '\r'|'\n'|';'|' ' -> true | _ -> false)

    between leftBracket rightBracket (many (spaces >>. recordEntry .>> newlineOrSemiColon))
    <?> "record"
    <!> "record"
    |>> (Map.ofList >> TemplateValue.Record)

let expressionBetweenParens =
    between (pchar '(') (pchar ')') expression
    <?> "expression between parens"
    <!> "expression between parens"

let variable =
    identifier <?> "variable"
    <!> "variable"
    |>> fun name -> SimpleValue(name)

let argumentExpressions = choice [
    attempt number
    attempt record
    attempt expressionBetweenParens
    attempt literal

    attempt variable
]

let opp = new OperatorPrecedenceParser<TemplateValue,unit,unit>()
let pipeExpression = opp.ExpressionParser
opp.TermParser <- attempt expression .>> spaces
opp.AddOperator(
    InfixOperator(
        "|>",
        spaces,
        1,
        Associativity.Left,
        fun left right ->
            match right with
            | SimpleValue fnName ->
                Function(fnName, [ left ])
            | Function (fn, args) ->
                Function(fn, args @ [ left ])
    )
)
    

let parseFunction =
    let commaSeparatedExpressions: Parser<TemplateValue list, unit> =
        sepBy1 expression (spaces >>. skipChar ',' >>. spaces) <?> "comma separated expression"
        <!> "comma separated expression"
        |>> fun x -> x

    let spaceSeparatedExpressions: Parser<TemplateValue list, unit> =
        sepEndBy1 argumentExpressions spaces <?> "space separated expression"
        <!> "space separated expression"
        |>> fun x -> x

    let emptyArguments =
        skipString "()" <?> "empty arguments"
        <!> "empty arguments"
        |>> fun _ -> []
    let parensArguments =
        skipChar '(' >>. spaces >>. commaSeparatedExpressions .>> spaces .>> skipChar ')' <?> "argument list"
        <!> "argument list"
        |>> fun expr -> expr
    let curriedArguments =
        spaces >>. spaceSeparatedExpressions .>> spaces <?> "curried arguments"
        <!> "curried arguments"

    let arguments =
        choice [
            attempt emptyArguments
            attempt parensArguments
            attempt curriedArguments
        ] <!> "arguments"
        
    (spaces >>. identifier) .>>. (spaces >>. arguments)
    <?> "function call"
    <!> "function call"
    |>> (fun (id, pars) -> Function(id, pars))

expressionImpl.Value <-
    choice [
        attempt record
        attempt expressionBetweenParens
        attempt literal
        attempt number
        attempt parseFunction

        attempt variable
    ] <!> "expression"

let parseTemplateValue text =
    let rec newParse t =
        t
        |> clean
        |> run (pipeExpression .>>? spaces .>> eof)
        |> function
        | ParserResult.Failure (err, _, _) ->
            None
        | ParserResult.Success (expression, _, _) ->
            Some expression
        |> function 
        | Some v -> v
        | None -> t |> SimpleValue

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