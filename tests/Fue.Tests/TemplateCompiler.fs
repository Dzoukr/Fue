module Fue.Tests.TemplateCompiler

open NUnit.Framework
open FsUnit
open Fue.Core
open Fue.Data
open Fue.TemplateCompiler
open Fue.Rop
open System.Globalization

[<Test>]
let ``Compiles single value`` () = 
    let data = init |> add "value" "Roman"
    "{{{value}}}"
    |> compile data
    |> should equal "Roman"

[<Test>]
let ``Compiles single value with spaces`` () = 
    let data = init |> add "value" "Roman"
    "A {{{  value   }}} B"
    |> compile data
    |> should equal "A Roman B"

[<Test>]
let ``Compiles the same value twice`` () = 
    let data = init |> add "value" "Roman"
    "{{{  value   }}}|{{{value}}}"
    |> compile data
    |> should equal "Roman|Roman"

[<Test>]
let ``Compiles two values`` () = 
    let data = init |> add "value" "Roman" |> add "value2" "Provaznik"
    "{{{  value   }}}|{{{value2}}}"
    |> compile data
    |> should equal "Roman|Provaznik"
