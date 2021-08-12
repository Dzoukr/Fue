module Fue.Tests.Issues

open NUnit.Framework
open FsUnit
open Fue.Data
open Fue.Compiler
open Fue.Parser
open Fue.Core

[<Test>]
let ``Supports numbers in literals (Issue #5)``() =
    let html = "{{{multiply 2}}}"
    init 
    |> add "multiply" (fun x -> x * 2)
    |> fromText html
    |> should equal "4"

[<Test>]
let ``Supports numbers in literals with more params (Issue #5)``() =
    let html = "{{{multiply 2 5}}}"
    init 
    |> add "multiply" (fun x y -> x * y)
    |> fromText html
    |> should equal "10"

[<Test>]
let ``Combines numbers and literals (Issue #5)``() =
    let html = "{{{multiply 'someText' 2 5}}}"
    init 
    |> add "multiply" (fun text x y -> x * y |> sprintf "This is %s and %i" text)
    |> fromText html
    |> should equal "This is someText and 10"


[<Test>]
let ``Parses correctly with spaces (Issue #4)``() =
    """ now() |> fmtDate "MMMM yyyy" """ 
    |> parseTemplateValue 
    |> should equal (
        TemplateValue.Function("fmtDate", 
            [
                TemplateValue.Literal("MMMM yyyy")
                TemplateValue.Function("now", [])
            ]))
    
[<Test>]
let ``Parse angel brackets correctly and do not add closing endtag (Issue #16)``() =
    let template = """<fs-template fs-if="render">ArrayList<Class<{{{change(obj)}}}>></fs-template>"""
    init
    |> add "obj" "Entity"
    |> add "change" (fun (x: string) -> x.ToUpper())
    |> add "render" true
    |> fromNoneHtmlText template
    |> should equal "ArrayList<Class<ENTITY>>"