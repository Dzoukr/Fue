module Fue.Tests.Compiler

open NUnit.Framework
open FsUnit
open Fue.Data
open Fue.Compiler
open System.IO
open System

let private getFileContent file = 
    Path.Combine([|AppDomain.CurrentDomain.BaseDirectory; file|])
    |> File.ReadAllText

[<Test>]
let ``Compiles the same value twice`` () = 
    let html = "{{{  value   }}}|{{{value}}}"
    init |> add "value" "Roman"
    |> fromText html
    |> should equal "Roman|Roman"

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
let ``Compiles else with new line`` () = 
    init |> add "b" false |> add "value" "Roman"
    |> fromText """<div fs-if="b">A</div>
    <div fs-else>{{{value}}}</div>"""
    |> should equal """
    <div>Roman</div>"""

[<Test>]
let ``Compiles else`` () = 
    init |> add "b" false
    |> fromText """<i fs-if="b" class="fa fa-sign-in text-success"></i><i fs-else class="fa fa-sign-out text-danger"></i>"""
    |> should equal """<i class="fa fa-sign-out text-danger"></i>"""


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

type MyCase =
    | A of string * string
    | B

[<Test>]
let ``Ignores extraction when specified for union`` () = 
    init 
    |> add "someCase" (MyCase.A("Ahoj","Zdar"))
    |> fromText """<div fs-du="someCase" fs-case="A">Ano</div>"""
    |> should equal "<div>Ano</div>"

[<Test>]
let ``Works with constant value`` () = 
    init 
    |> add "myFunc" (fun value -> value + " added")
    |> fromText """{{{"something" |> myFunc}}}"""
    |> should equal "something added"

[<Test>]
let ``Works if condition with constant value`` () =
    init 
    |> add "equals" (fun x y -> x = y) |> add "item" "menu"
    |> fromText """<div fs-if="equals(item, 'menu')">Yes</div>"""
    |> should equal "<div>Yes</div>"

[<Test>]
let ``Ommites fs-template tag`` () =
    init 
    |> fromText """<fs-template>Yes</fs-template>"""
    |> should equal "Yes"

[<Test>]
let ``Ommites fs-template tag with inner html`` () =
    init 
    |> fromText """<fs-template><div>Yes</div></fs-template>"""
    |> should equal "<div>Yes</div>"

[<Test>]
let ``Ommites fs-template tag with if`` () =
    init
    |> add "bool" true
    |> fromText """<fs-template fs-if="bool"><div>Yes</div></fs-template>"""
    |> should equal "<div>Yes</div>"

type Nested = {
    Value : int
}

type Record = {
    Name : string
    Age : int
    Nested : Nested
}
with 
    member this.Show = this.Name
    member this.ShowFunc() = this.Name
    member this.Calc x = x * 2

type RecordWithFun = {
    Fun : int -> int
    NoParam: unit -> string
}

type NestedClass() =
    member this.Value = "Nested"

type Class() =
    member this.Name = "Roman"
    member this.Nested = new NestedClass()
    static member Name_Static = "Roman Static"
    member this.Y(x) = x * 2
    static member YStatic(x) = x * 2

[<Test>]
let ``Compiles class value`` () = 
    init |> add "cls" (new Class())
    |> fromText "{{{cls.Name}}}"
    |> should equal "Roman"

[<Test>]
let ``Compiles class method`` () = 
    let cls = new Class()
    init |> add "cls" cls |> add "x" 10
    |> fromText "{{{cls.Y(x)}}}"
    |> should equal "20"

[<Test>]
let ``Compiles class static method`` () = 
    let cls = new Class()
    init |> add "cls" cls |> add "x" 10
    |> fromText "{{{cls.YStatic(x)}}}"
    |> should equal "20"

[<Test>]
let ``Compiles class static value`` () = 
    let cls = new Class()
    init |> add "cls" cls
    |> fromText "{{{cls.Name_Static}}}"
    |> should equal "Roman Static"

[<Test>]
let ``Compiles nested class value`` () = 
    let cls = new Class()
    init |> add "cls" cls
    |> fromText "{{{cls.Nested.Value}}}"
    |> should equal "Nested"

[<Test>]
let ``Compiles record value`` () = 
    let record = { Name = "Roman"; Age = 35; Nested = { Value = 123 } }
    init |> add "rec" record
    |> fromText "{{{rec.Name}}}"
    |> should equal "Roman"

[<Test>]
let ``Compiles record member`` () = 
    let record = { Name = "Roman"; Age = 35; Nested = { Value = 123 } }
    init |> add "rec" record
    |> fromText "{{{rec.Show}}}"
    |> should equal "Roman"

[<Test>]
let ``Compiles record member parameterless method`` () = 
    let record = { Name = "Roman"; Age = 35; Nested = { Value = 123 } }
    init |> add "rec" record
    |> fromText "{{{rec.ShowFunc()}}}"
    |> should equal "Roman"

[<Test>]
let ``Compiles record member method with params`` () = 
    let record = { Name = "Roman"; Age = 35; Nested = { Value = 123 } }
    init |> add "rec" record |> add "y" 50
    |> fromText "{{{rec.Calc(y)}}}"
    |> should equal "100"

[<Test>]
let ``Compiles record with function`` () = 
    let record = { Fun = (fun x -> x + 10); NoParam = (fun() -> "Roman") }
    init |> add "rec" record |> add "param" 90
    |> fromText "{{{param |> rec.Fun}}}"
    |> should equal "100"

[<Test>]
let ``Compiles record with parameterless function`` () = 
    let record = { Fun = (fun x -> x + 10); NoParam = (fun() -> "Roman") }
    init |> add "rec" record |> add "param" 90
    |> fromText "{{{rec.NoParam()}}}"
    |> should equal "Roman"

[<Test>]
let ``Compiles nested record value`` () = 
    let record = { Name = "Roman"; Age = 35; Nested = { Value = 123 } }
    init |> add "rec" record
    |> fromText "{{{rec.Nested.Value}}}"
    |> should equal "123"

[<Test>]
let ``Compiles tuple function`` () = 
    let tuple = "Roman", 35
    init |> add "tuple" tuple |> add "fst" fst
    |> fromText "{{{fst (tuple)}}}"
    |> should equal "Roman"

[<Test>]
let ``Compiles parameterless function`` () = 
    let fun1 = fun() -> "Roman"
    init |> add "fun1" fun1
    |> fromText "{{{fun1()}}}"
    |> should equal "Roman"

[<Test>]
let ``Compiles function with param`` () = 
    let fun1 = fun x -> x + 10
    init |> add "fun1" fun1 |> add "x" 90
    |> fromText "{{{fun1(x)}}}"
    |> should equal "100"

[<Test>]
let ``Compiles function with more params`` () = 
    let fun1 = fun x y -> x + y
    init |> add "fun1" fun1 |> add "x" 90 |> add "y" 10
    |> fromText "{{{fun1(x,y)}}}"
    |> should equal "100"

[<Test>]
[<Ignore("Issue investigated")>]
let ``Compiles function with nested functions`` () = 
    let addFun = fun x y -> x + y
    let multFun x y = x * y 
    init |> add "add" addFun |> add "mult" multFun |> add "x" 3 |> add "y" 2 |> add "z" 5
    |> fromText "{{{add(x,mult(y,z))}}}"
    |> should equal "13"

[<Test>]
let ``Compiles Some and None`` () = 
    init |> add "someValue" (Some "my value")
    |> fromText "{{{someValue.IsSome}}}"
    |> should equal "True"

[<Test>]
let ``Compiles generic function`` () = 
    init |> add "someFunc" (fun x -> x) |> add "x" 10
    |> fromText "{{{someFunc(x)}}}"
    |> should equal "10"

[<Test>]
let ``Compiles function with constant value`` () = 
    init |> add "print" (fun x -> "printed " + x)
    |> fromText "{{{print('hello')}}}"
    |> should equal "printed hello"