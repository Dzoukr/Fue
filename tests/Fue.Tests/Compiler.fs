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
    let data = init |> add "who" "Roman" |> add "me" "Dzoukr"
    html 
    |> compileFromString data
    |> should equal """<div id="Roman">Dzoukr</div>"""

[<Test>]
let ``Compiles from unvalid html string`` () = 
    let html = """No root here {{{who}}}<div id="{{{who}}}">{{{me}}}</div>"""
    let data = init |> add "who" "Roman" |> add "me" "Dzoukr"
    html 
    |> compileFromString data
    |> should equal """No root here Roman<div id="Roman">Dzoukr</div>"""

[<Test>]
let ``Compiles from plain string`` () = 
    let html = """Hi {{{who}}}"""
    let data = init |> add "who" "Dzoukr"
    html 
    |> compileFromString data
    |> should equal """Hi Dzoukr"""

[<Test>]
let ``Compiles from file`` () = 
    let data = init |> add "message" "Dzoukr"
    let result = compileFromFile data "SimplePage.html"
    result |> should equal ("SimplePageCompiled.html" |> getFileContent)