module Fue.Tests.Parser

open NUnit.Framework
open FsUnit
open Fue.Core
open Fue.Parser
open Fue.Rop
open HtmlAgilityPack

[<Test>]
let ``Parses simple value`` () = 
    "value" 
    |> parseTemplateValue 
    |> should equal (TemplateValue.SimpleValue("value"))

[<Test>]
let ``Parses constant value`` () = 
    """ "value" """
    |> parseTemplateValue 
    |> should equal (TemplateValue.Constant("value"))

[<Test>]
let ``Parses constant value (single quote)`` () = 
    "'value'"
    |> parseTemplateValue 
    |> should equal (TemplateValue.Constant("value"))

[<Test>]
let ``Parses function value`` () = 
    "value()" 
    |> parseTemplateValue 
    |> should equal (TemplateValue.Function("value", []))

[<Test>]
let ``Parses function with constant value`` () = 
    "value(\"hello\")" 
    |> parseTemplateValue 
    |> should equal (TemplateValue.Function("value", [TemplateValue.Constant("hello")]))

[<Test>]
let ``Parses function with constant value and simple value`` () = 
    "equals(my, \"hello\")" 
    |> parseTemplateValue 
    |> should equal (TemplateValue.Function("equals", [TemplateValue.SimpleValue("my"); TemplateValue.Constant("hello")]))

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

let parseNodeSuccess = parseNode >> extract >> Option.get

[<Test>]
let ``Parses for cycle node`` () = 
    let expected = TemplateNode.ForCycle("i", TemplateValue.SimpleValue("list"))
    HtmlNode.CreateNode """<a fs-for="i in list" """
    |> parseNodeSuccess 
    |> should equal expected

[<Test>]
let ``Parses if condition node`` () = 
    let expected = TemplateNode.IfCondition(TemplateValue.SimpleValue("boolVal"))
    HtmlNode.CreateNode """<a fs-if="boolVal" """
    |> parseNodeSuccess 
    |> should equal expected

[<Test>]
let ``Parses else condition node`` () = 
    let expected = TemplateNode.ElseCondition
    HtmlNode.CreateNode """<a fs-else href="abc" """
    |> parseNodeSuccess 
    |> should equal expected

[<Test>]
let ``Parses discriminated union node`` () = 
    let expected = TemplateNode.DiscriminatedUnion(TemplateValue.SimpleValue("union"), "case", ["a";"_"])
    HtmlNode.CreateNode """<a fs-du="union" fs-case="case(a,_)" """
    |> parseNodeSuccess 
    |> should equal expected

[<Test>]
let ``Parses text interpolations`` () = 
    let expected = [("{{{abc}}}","abc");("{{{ def }}}","def")]
    "{{{abc}}} xxx {{{ def }}} {{{}}} {{{   }}}"
    |> parseTextInterpolations 
    |> should equal expected
