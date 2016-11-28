module Fue.Tests.Parser

open NUnit.Framework
open FsUnit
open Fue.Core
open Fue.Parser

[<Test>]
let ``Parses simple value`` () = 
    "value" 
    |> parseTemplateValue 
    |> should equal (TemplateValue.SimpleValue("value"))

[<Test>]
let ``Parses function value`` () = 
    "value()" 
    |> parseTemplateValue 
    |> should equal (TemplateValue.Function("value", []))

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
let ``Parses for-cycle`` () = 
    "x in y" 
    |> parseForCycle 
    |> should equal (TemplateNode.ForCycle("x", TemplateValue.SimpleValue("y")) |> Some)

[<Test>]
let ``Parses for-cycle with function`` () = 
    "x in y(z)" 
    |> parseForCycle 
    |> should equal (TemplateNode.ForCycle("x", TemplateValue.Function("y", [TemplateValue.SimpleValue("z")])) |> Some)

[<Test>]
let ``Does not parse illegal for-cycle`` () = 
    "in y" 
    |> parseForCycle 
    |> should equal None