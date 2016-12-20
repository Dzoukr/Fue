module Fue.Tests.Compiler

open NUnit.Framework
open FsUnit
open Fue.Core
open Fue.Data
open Fue.Compiler
open Fue.Rop
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

[<Test>]
let ``Compiles correctly html entities`` () =
    init |> add "status" "OK"
    |> fromText """<span class="label label-info"><i class="fa fa-clock-o"></i>&nbsp;{{{status}}}</span>"""
    |> should equal """<span class="label label-info"><i class="fa fa-clock-o"></i>&nbsp;OK</span>"""

[<Test>]
let ``Compiles true if`` () = 
    init |> add "b" true |> add "value" "Roman"
    |> fromText """<div fs-if="b">{{{value}}}</div>"""
    |> should equal "<div>Roman</div>"

[<Test>]
let ``Compiles false if`` () = 
    init |> add "b" false
    |> fromText """<div fs-if="b">{{{value}}}</div>"""
    |> should equal ""

[<Test>]
let ``Compiles if not`` () = 
    init |> add "b" false |> add "value" "Roman"
    |> fromText """<div fs-if-not="b">{{{value}}}</div>"""
    |> should equal "<div>Roman</div>"

[<Test>]
let ``Compiles for from list`` () = 
    init |> add "y" ["a";"b";"c"]
    |> fromText """<div fs-for="x in y">{{{x}}}</div>"""
    |> should equal "<div>a</div><div>b</div><div>c</div>"

[<Test>]
let ``Compiles for from array`` () = 
    init |> add "y" [|"a";"b";"c"|]
    |> fromText """<div fs-for="x in y">{{{x}}}</div>"""
    |> should equal "<div>a</div><div>b</div><div>c</div>"

[<Test>]
let ``Compiles for from seq function`` () = 
    init |> add "y" (fun () -> seq { for i in [1..3] do yield i } |> Seq.toList)
    |> fromText """<div fs-for="x in y()">{{{x}}}</div>"""
    |> should equal "<div>1</div><div>2</div><div>3</div>"

type MyUnion =
    | Admin
    | User of string * int

[<Test>]
let ``Compiles discriminated union`` () = 
    init |> add "src" (MyUnion.User("Roman",42))
    |> fromText """<div id="82" fs-du="src" fs-case="User(x,y)">{{{y}}},{{{x}}}</div>"""
    |> should equal """<div id="82">42,Roman</div>"""

[<Test>]
let ``Ignores discriminated union with different case`` () = 
    init |> add "src" (MyUnion.Admin)
    |> fromText """START<div fs-du="src" fs-case="User(x,y)">{{{y}}},{{{x}}}</div>"""
    |> should equal "START"

[<Test>]
let ``Compiles attribute values`` () = 
    init |> add "who" "Roman" |> add "me" "Dzoukr"
    |> fromText """<div id="{{{who}}}">{{{me}}}</div>"""
    |> should equal """<div id="Roman">Dzoukr</div>"""

[<Test>]
let ``Compiles additional info for forcycle`` () = 
    init |> add "items" ["A";"B";"C"]
    |> fromFile "ForCyclePage.html"
    |> should equal ("ForCyclePageCompiled.html" |> getFileContent)

[<Test>]
let ``Compiles with complex html`` () = 
    init 
    |> add "menu" """<!-- MENU --><div class="menu"><a href="javascript:;">Test</a></div>"""
    |> fromFile "MasterPage.html"
    |> should equal ("MasterPageCompiled.html" |> getFileContent)

type MyModel = {
    Name : string
    Surname : string option
}

[<Test>]
let ``Compiles with Some directly`` () = 
    init 
    |> add "value" { Name = "AAA"; Surname = Some("bbb")}
    |> fromText """<div fs-if="value.Surname.IsSome">Ano</div>"""
    |> should equal "<div>Ano</div>"

[<Test>]
let ``Compiles with None directly`` () = 
    init 
    |> add "value" { Name = "AAA"; Surname = None}
    |> fromText """<div fs-if="value.Surname.IsSome">Ano</div>"""
    |> should equal ""

[<Test>]
let ``Compiles with Some as external`` () = 
    init 
    |> add "value" (Some "string")
    |> add "isSome" Option.isSome<string>
    |> fromText """<div fs-if="value |> isSome">Ano</div>"""
    |> should equal "<div>Ano</div>"

type MyModels = {
    Models : MyModel list
}

[<Test>]
let ``Compiles nested loops`` () = 
    let models = [0..3] |> List.map (fun x -> {Name = "A" + x.ToString(); Surname = Some("AA" + x.ToString())})
    
    init 
    |> add "value" (Some "string")
    |> add "isSome" Option.isSome<string>
    |> fromText """<div fs-if="value |> isSome">Ano</div>"""
    |> should equal "<div>Ano</div>"
