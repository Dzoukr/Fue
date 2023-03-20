module Fue.Tests.Parser

open NUnit.Framework
open FsUnit
open Fue.Core
open Fue.Parser
open HtmlAgilityPack

let simpleValue = "value"
let literal = """ "value" """
let multilineLiteral = """ "first
second" """
let singleQuoteLiteral = "'value'"
let trippleQuotedLiteral = " \"\"\"value\"\"\" "
let trippleQuotedLiteralMultiLine = " \"\"\"value

with mutliple lines
\"\"\" "
let singleLineRecord = "{ test = \"123\" }"
let singleLineRecordNoSpace = "{test=\"123\"}"
let singleLineRecord2 = "{ test = \"123\"; other = \"abc\" }"

let multiLineRecord = "{
    test = \"123\"
}"
let multiLineRecord2 = "{
    test = \"123\"
    other = \"abc\"
}"
let multiLineRecordWithFunction = "{
    other = func()
}"
let multiLineRecordWithFunctionAndParameter = "{
    other = func(\"test\")
}"
let multiLineRecordWithFunctionAndParameterVariable = "{
    other = func(test)
}"
let multiLineRecordWithPipedFunctionAndParameterVariable = "{
    pipe = test |> func
    multiPipe = test |> func1 \"param\" |> func2
}"



[<Test>]
let ``Parses simple value`` () = 
    simpleValue
    |> parseTemplateValue 
    |> should equal (TemplateValue.SimpleValue("value"))

[<Test>]
let ``Parses literal value`` () = 
    literal
    |> parseTemplateValue 
    |> should equal (TemplateValue.Literal("value"))

[<Test>]
let ``Parses literal value (multi line)`` () = 
    multilineLiteral
    |> parseTemplateValue 
    |> should equal (TemplateValue.Literal("first\nsecond"))

[<Test>]
let ``Parses literal value (triple quote)`` () = 
    trippleQuotedLiteral
    |> parseTemplateValue 
    |> should equal (TemplateValue.Literal("value"))

[<Test>]
let ``Parses literal value (triple quote multiline)`` () = 
    trippleQuotedLiteralMultiLine
    |> parseTemplateValue 
    |> should equal (TemplateValue.Literal("value\n\nwith mutliple lines\n"))

[<Test>]
let ``Parses literal value (single quote)`` () = 
    singleQuoteLiteral
    |> parseTemplateValue 
    |> should equal (TemplateValue.Literal("value"))


[<Test>]
let ``Parses record (single line)`` () = 
    let result =
        Map.empty
        |> Map.add "test" (TemplateValue.Literal("123"))

    singleLineRecord
    |> parseTemplateValue 
    |> should equal (TemplateValue.Record(result))

[<Test>]
let ``Parses record (single line no space)`` () = 
    let result =
        Map.empty
        |> Map.add "test" (TemplateValue.Literal("123"))

    singleLineRecordNoSpace
    |> parseTemplateValue 
    |> should equal (TemplateValue.Record(result))

[<Test>]
let ``Parses record (single line multiple entries)`` () = 
    let result =
        Map.empty
        |> Map.add "test" (TemplateValue.Literal("123"))
        |> Map.add "other" (TemplateValue.Literal("abc"))

    singleLineRecord2
    |> parseTemplateValue 
    |> should equal (TemplateValue.Record(result))

[<Test>]
let ``Parses record (multi line)`` () =
    let result =
        Map.empty
        |> Map.add "test" (TemplateValue.Literal("123"))

    multiLineRecord
    |> parseTemplateValue 
    |> should equal (TemplateValue.Record(result))

[<Test>]
let ``Parses record (multi line, multiple entries)`` () =
    let result =
        Map.empty
        |> Map.add "test" (TemplateValue.Literal("123"))
        |> Map.add "other" (TemplateValue.Literal("abc"))

    multiLineRecord2
    |> parseTemplateValue 
    |> should equal (TemplateValue.Record(result))

[<Test>]
let ``Parses record with function call`` () =
    let result =
        Map.empty
        |> Map.add "other" (TemplateValue.Function("func", []))

    multiLineRecordWithFunction
    |> parseTemplateValue 
    |> should equal (TemplateValue.Record(result))

[<Test>]
let ``Parses record with function call (and parameter)`` () =
    let result =
        Map.empty
        |> Map.add "other" (TemplateValue.Function("func", [ TemplateValue.Literal("test")]))

    multiLineRecordWithFunctionAndParameter
    |> parseTemplateValue 
    |> should equal (TemplateValue.Record(result))

[<Test>]
let ``Parses record with function call (and variable)`` () =
    let result =
        Map.empty
        |> Map.add "other" (TemplateValue.Function("func", [ TemplateValue.SimpleValue("test")]))

    multiLineRecordWithFunctionAndParameterVariable
    |> parseTemplateValue 
    |> should equal (TemplateValue.Record(result))

[<Test>]
let ``Parses record with piped function call (and variable)`` () =
    let result =
        Map.empty
        |> Map.add "pipe" (TemplateValue.Function("func", [ TemplateValue.SimpleValue("test")]))
        |> Map.add "multiPipe" (TemplateValue.Function("func2", [
            TemplateValue.Function("func1", [ TemplateValue.Literal("param"); TemplateValue.SimpleValue("test")])
        ]))

    multiLineRecordWithPipedFunctionAndParameterVariable
    |> parseTemplateValue 
    |> should equal (TemplateValue.Record(result))

[<Test>]
let ``Parses function value`` () = 
    "value()" 
    |> parseTemplateValue 
    |> should equal (TemplateValue.Function("value", []))

[<Test>]
let ``Parses function value (with param)`` () = 
    "value param" 
    |> parseTemplateValue 
    |> should equal (TemplateValue.Function("value", [ TemplateValue.SimpleValue("param")]))

[<Test>]
let ``Parses function value (with more params)`` () = 
    "value param param2"
    |> parseTemplateValue 
    |> should equal (TemplateValue.Function("value", [ TemplateValue.SimpleValue("param"); TemplateValue.SimpleValue("param2")]))

[<Test>]
let ``Parses function value (with literal value)`` () = 
    "value(\"hello\")" 
    |> parseTemplateValue 
    |> should equal (TemplateValue.Function("value", [TemplateValue.Literal("hello")]))

[<Test>]
let ``Parses function value (with reference)`` () = 
    "value(hello)" 
    |> parseTemplateValue 
    |> should equal (TemplateValue.Function("value", [TemplateValue.SimpleValue("hello")]))

[<Test>]
let ``Parses function value (with multiple references)`` () = 
    "value(hello, xy)" 
    |> parseTemplateValue 
    |> should equal (TemplateValue.Function("value", [TemplateValue.SimpleValue("hello"); TemplateValue.SimpleValue("xy")]))

[<Test>]
let ``Parses function value (with multiple literals)`` () = 
    "value(\"hello\", \"heyo\",\"heyo\" ,\"ugh\")" 
    |> parseTemplateValue 
    |> should equal (TemplateValue.Function("value", [
        TemplateValue.Literal("hello")
        TemplateValue.Literal("heyo")
        TemplateValue.Literal("heyo")
        TemplateValue.Literal("ugh")
    ]))

[<Test>]
let ``Parses function with literal value (triple quoted)`` () = 
    "value(\"\"\"hello\"\"\")" 
    |> parseTemplateValue 
    |> should equal (TemplateValue.Function("value", [TemplateValue.Literal("hello")]))

[<Test>]
let ``Parses function with literal value (triple quoted multi line)`` () = 
    "value(\"\"\"first
second\"\"\")" 
    |> parseTemplateValue 
    |> should equal (TemplateValue.Function("value", [TemplateValue.Literal("first\nsecond")]))

[<Test>]
let ``Parses function with record`` () = 
    let result =
        Map.empty
        |> Map.add "test" (TemplateValue.Literal("hello"))

    "value({ test = \"hello\" })" 
    |> parseTemplateValue 
    |> should equal (TemplateValue.Function("value", [TemplateValue.Record(result)]))

[<Test>]
let ``Parses function with record (multi line)`` () = 
    let result =
        Map.empty
        |> Map.add "test" (TemplateValue.Literal("hello"))
        |> Map.add "testAB" (TemplateValue.Literal("Hy123"))

    "value({
    test = \"hello\"
    testAB = \"Hy123\"
})" 
    |> parseTemplateValue 
    |> should equal (TemplateValue.Function("value", [TemplateValue.Record(result)]))

[<Test>]
let ``Parses function with tuple and record (multi line)`` () = 
    let result =
        Map.empty
        |> Map.add "test" (TemplateValue.Literal("hello"))
        |> Map.add "testAB" (TemplateValue.Literal("Hy123"))

    "value(\"first\", {
    test = \"hello\"
    testAB = \"Hy123\"
})" 
    |> parseTemplateValue 
    |> should equal (TemplateValue.Function("value", [TemplateValue.Literal("first"); TemplateValue.Record(result)]))

[<Test>]
let ``Parses piped function with literal value`` () = 
    "x |> value 'first'" 
    |> parseTemplateValue 
    |> should equal (TemplateValue.Function("value", [TemplateValue.Literal("first"); TemplateValue.SimpleValue("x")]))

[<Test>]
let ``Can't pipe into literal`` () = 
    (fun () ->
        "x |> 'first'" 
        |> parseTemplateValue 
        |> ignore
    )
    |> should throw typeof<System.Exception>

[<Test>]
let ``Parses function with literal value and simple value`` () = 
    "equals(my, \"hello\")" 
    |> parseTemplateValue 
    |> should equal (TemplateValue.Function("equals", [TemplateValue.SimpleValue("my"); TemplateValue.Literal("hello")]))

[<Test>]
let ``Parses method`` () = 
    "record.Method()" 
    |> parseTemplateValue 
    |> should equal (TemplateValue.Function("record.Method", []))

[<Test>]
let ``Parses literal piped into function`` () = 
    "\"value\" |>fun1 " 
    |> parseTemplateValue 
    |> should equal (
        TemplateValue.Function("fun1", 
            [
                TemplateValue.Literal("value")
            ]))

[<Test>]
let ``Parses piped function value`` () = 
    "value |> fun1 |> fun2" 
    |> parseTemplateValue 
    |> should equal (
        TemplateValue.Function("fun2", 
            [
                TemplateValue.Function("fun1", [TemplateValue.SimpleValue("value")])
            ]))

[<Test>]
let ``Parses piped curried function value`` () = 
    "value |> fun1 y |> fun2 x" 
    |> parseTemplateValue 
    |> should equal (
        TemplateValue.Function("fun2", 
            [
                TemplateValue.SimpleValue("x")
                TemplateValue.Function("fun1", [TemplateValue.SimpleValue("y"); TemplateValue.SimpleValue("value")])
            ]))

[<Test>]
let ``Parses piped double curried function value`` () = 
    "value |> fun1 x y z |> fun2 x" 
    |> parseTemplateValue 
    |> should equal (
        TemplateValue.Function("fun2", 
            [
                TemplateValue.SimpleValue("x")
                TemplateValue.Function("fun1", [TemplateValue.SimpleValue("x"); TemplateValue.SimpleValue("y");TemplateValue.SimpleValue("z"); TemplateValue.SimpleValue("value")])
            ]))

[<Test>]
let ``Parses piped curried function value with first function`` () = 
    "value() |> fun1 y" 
    |> parseTemplateValue 
    |> should equal (
        TemplateValue.Function("fun1", 
            [
                TemplateValue.SimpleValue("y")
                TemplateValue.Function("value", [])
            ]))

[<Test>]
let ``Parses function value with params`` () = 
    "value(a,b)" 
    |> parseTemplateValue 
    |> should equal (
        TemplateValue.Function("value", 
            [TemplateValue.SimpleValue("a"); TemplateValue.SimpleValue("b")]))

[<Test>]
let ``Parses function value with inner function`` () = 
    "value(a,b(x),c())" 
    |> parseTemplateValue 
    |> should equal (
        TemplateValue.Function("value", 
            [
                TemplateValue.SimpleValue("a"); 
                TemplateValue.Function("b", [TemplateValue.SimpleValue("x")]);
                TemplateValue.Function("c", []);
            ]))

[<Test>]
let ``Parses function value with inner function (different format)`` () = 
    "add(x,mult(y,z))" 
    |> parseTemplateValue 
    |> should equal (
        TemplateValue.Function("add", 
            [
                TemplateValue.SimpleValue("x"); 
                TemplateValue.Function("mult", [TemplateValue.SimpleValue("y");TemplateValue.SimpleValue("z")]);
            ]))

[<Test>]
let ``Parses simple value with white spaces`` () = 
    "value (a, b)" 
    |> parseTemplateValue 
    |> should equal (
        TemplateValue.Function("value", 
            [TemplateValue.SimpleValue("a"); TemplateValue.SimpleValue("b")]))

[<Test>]
let ``Parses for-cycle value`` () = 
    "x in y" 
    |> parseForCycleAttribute 
    |> should equal (TemplateNode.ForCycle("x", TemplateValue.SimpleValue("y")) |> Some)

[<Test>]
let ``Parses for-cycle value with function`` () = 
    "x in y(z)" 
    |> parseForCycleAttribute 
    |> should equal (TemplateNode.ForCycle("x", TemplateValue.Function("y", [TemplateValue.SimpleValue("z")])) |> Some)

[<Test>]
let ``Parses for-cycle value with piped function`` () = 
    "x in y |> z 'lit'" 
    |> parseForCycleAttribute 
    |> should equal (
        TemplateNode.ForCycle("x", 
            TemplateValue.Function("z", [TemplateValue.Literal("lit"); TemplateValue.SimpleValue("y")])) |> Some)

[<Test>]
let ``Does not parse illegal for-cycle value`` () = 
    "in y" 
    |> parseForCycleAttribute 
    |> should equal None

[<Test>]
let ``Parses discriminated union case with no extraction`` () = 
    let res = "Case" |> parseUnionCaseAttribute
    res |> fst |> should equal "Case"
    res |> snd |> should be Empty
    
[<Test>]
let ``Parses discriminiated case with extract`` () = 
    "Case(x, _)" 
    |> parseUnionCaseAttribute
    |> should equal ("Case", ["x";"_"])

let parseNode = parseNode >> Option.get

[<Test>]
let ``Parses for cycle node`` () = 
    let expected = TemplateNode.ForCycle("i", TemplateValue.SimpleValue("list"))
    HtmlNode.CreateNode """<a fs-for="i in list" """
    |> parseNode 
    |> should equal expected

[<Test>]
let ``Parses if condition node`` () = 
    let expected = TemplateNode.IfCondition(TemplateValue.SimpleValue("boolVal"))
    HtmlNode.CreateNode """<a fs-if="boolVal" """
    |> parseNode 
    |> should equal expected

[<Test>]
let ``Parses else condition node`` () = 
    let expected = TemplateNode.ElseCondition
    HtmlNode.CreateNode """<a fs-else href="abc" """
    |> parseNode 
    |> should equal expected

[<Test>]
let ``Parses discriminated union node`` () = 
    let expected = TemplateNode.DiscriminatedUnion(TemplateValue.SimpleValue("union"), "case", ["a";"_"])
    HtmlNode.CreateNode """<a fs-du="union" fs-case="case(a,_)" """
    |> parseNode 
    |> should equal expected

[<Test>]
let ``Parses text interpolations`` () = 
    let expected = [("{{{abc}}}","abc");("{{{ def }}}","def")]
    "{{{abc}}} xxx {{{ def }}} {{{}}} {{{   }}}"
    |> parseTextInterpolations 
    |> should equal expected

[<Test>]
let ``Parses nested records`` () = 
    let expected = Record <| Map [("number", Literal "123"); ("nestedContent", Record <| Map [("foo", Literal "bar")])]
    "{
        number = \"123\"
        nestedContent = {
            foo = \"bar\"
        }
    }"
    |> parseTemplateValue 
    |> should equal expected