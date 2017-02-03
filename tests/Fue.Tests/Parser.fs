module Fue.Tests.Parser

open NUnit.Framework
open FsUnit
open Fue.Core
open Fue.Parser
open HtmlAgilityPack

[<Test>]
let ``Parses simple value`` () = 
    "value" 
    |> parseTemplateValue 
    |> should equal (TemplateValue.SimpleValue("value"))

[<Test>]
let ``Parses literal value`` () = 
    """ "value" """
    |> parseTemplateValue 
    |> should equal (TemplateValue.Literal("value"))

[<Test>]
let ``Parses literal value (single quote)`` () = 
    "'value'"
    |> parseTemplateValue 
    |> should equal (TemplateValue.Literal("value"))

[<Test>]
let ``Parses function value`` () = 
    "value()" 
    |> parseTemplateValue 
    |> should equal (TemplateValue.Function("value", []))

[<Test>]
let ``Parses function value with param`` () = 
    "value param" 
    |> parseTemplateValue 
    |> should equal (TemplateValue.Function("value", [ TemplateValue.SimpleValue("param")]))

[<Test>]
let ``Parses function value with more params`` () = 
    "value param param2" 
    |> parseTemplateValue 
    |> should equal (TemplateValue.Function("value", [ TemplateValue.SimpleValue("param"); TemplateValue.SimpleValue("param2")]))

[<Test>]
let ``Parses function with literal value`` () = 
    "value(\"hello\")" 
    |> parseTemplateValue 
    |> should equal (TemplateValue.Function("value", [TemplateValue.Literal("hello")]))

[<Test>]
let ``Parses piped function with literal value`` () = 
    "x |> value 'first'" 
    |> parseTemplateValue 
    |> should equal (TemplateValue.Function("value", [TemplateValue.Literal("first"); TemplateValue.SimpleValue("x")]))

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
    res |> snd |> should equal []
    
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
