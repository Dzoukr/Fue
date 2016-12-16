module Fue.Tests.Compiler

open NUnit.Framework
open FsUnit
open Fue.Core
open Fue.Data
open Fue.Compiler
open Fue.Rop
open FSharp.Data

[<Test>]
let ``Compiles from string`` () = 
    let html = """<div id="{{{who}}}">{{{me}}}</div>"""
    let data = init |> add "who" "Roman" |> add "me" "Dzoukr"
    html 
    |> compileFromString data
    |> should equal """<div id="Roman">Dzoukr</div>"""