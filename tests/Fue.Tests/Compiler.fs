module Fue.Tests.Compiler

open NUnit.Framework
open FsUnit
open Fue.Core
open Fue.Data
open Fue.Compiler
open Fue.Rop
open FSharp.Data
open System.IO
open System

let private getFileContent file = 
    Path.Combine([|AppDomain.CurrentDomain.BaseDirectory; file|])
    |> File.ReadAllText

[<Test>]
let ``Compiles from string`` () = 
    let html = """<div id="{{{who}}}">{{{me}}}</div>"""
    init |> add "who" "Roman" |> add "me" "Dzoukr"
    |> fromText html
    |> should equal """<div id="Roman">Dzoukr</div>"""

[<Test>]
let ``Compiles from unvalid html string`` () = 
    let html = """No root here {{{who}}}<div id="{{{who}}}">{{{me}}}</div>"""
    init |> add "who" "Roman" |> add "me" "Dzoukr"
    |> fromText html
    |> should equal """No root here Roman<div id="Roman">Dzoukr</div>"""

[<Test>]
let ``Compiles from plain string`` () = 
    let html = """Hi {{{who}}}"""
    init |> add "who" "Dzoukr"
    |> fromText html
    |> should equal """Hi Dzoukr"""

[<Test>]
let ``Compiles with comments`` () = 
    let html = """<!-- jQuery (necessary for Bootstrap's JavaScript {{{who}}}) -->"""
    init |> add "who" "Dzoukr"
    |> fromText html
    |> should equal """<!-- jQuery (necessary for Bootstrap's JavaScript Dzoukr) -->"""


[<Test>]
let ``Compiles from file`` () = 
    init |> add "message" "Dzoukr" |> add "isTrue" true
    |> fromFile "SimplePage.html"
    |> should equal ("SimplePageCompiled.html" |> getFileContent)