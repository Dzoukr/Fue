module Fue.Tests.Compiler

open NUnit.Framework
open FsUnit
open Fue.Core
open Fue.Data
open Fue.Compiler

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
let ``Compiles simple value`` () = 
    let data = init |> add "value" "Roman"
    SimpleValue("value")
    |> compile data
    |> should equal "Roman"

[<Test>]
let ``Compiles class value`` () = 
    let cls = new Class()
    let data = init |> add "cls" cls
    SimpleValue("cls.Name")
    |> compile data
    |> should equal "Roman"

[<Test>]
let ``Compiles class method`` () = 
    let cls = new Class()
    let data = init |> add "cls" cls |> add "x" 10
    Function("cls.Y", [SimpleValue("x")])
    |> compile data
    |> should equal 20

[<Test>]
let ``Compiles class static method`` () = 
    let cls = new Class()
    let data = init |> add "cls" cls |> add "x" 10
    Function("cls.YStatic", [SimpleValue("x")])
    |> compile data
    |> should equal 20

[<Test>]
let ``Compiles class static value`` () = 
    let cls = new Class()
    let data = init |> add "cls" cls
    SimpleValue("cls.Name_Static")
    |> compile data
    |> should equal "Roman Static"

[<Test>]
let ``Compiles nested class value`` () = 
    let cls = new Class()
    let data = init |> add "cls" cls
    SimpleValue("cls.Nested.Value")
    |> compile data
    |> should equal "Nested"

[<Test>]
let ``Compiles record value`` () = 
    let record = { Name = "Roman"; Age = 35; Nested = { Value = 123 } }
    let data = init |> add "rec" record
    SimpleValue("rec.Name")
    |> compile data
    |> should equal "Roman"

[<Test>]
let ``Compiles record member`` () = 
    let record = { Name = "Roman"; Age = 35; Nested = { Value = 123 } }
    let data = init |> add "rec" record
    SimpleValue("rec.Show")
    |> compile data
    |> should equal "Roman"

[<Test>]
let ``Compiles record member parameterless method`` () = 
    let record = { Name = "Roman"; Age = 35; Nested = { Value = 123 } }
    let data = init |> add "rec" record
    Function("rec.ShowFunc", [])
    |> compile data
    |> should equal "Roman"

[<Test>]
let ``Compiles record member method with params`` () = 
    let record = { Name = "Roman"; Age = 35; Nested = { Value = 123 } }
    let data = init |> add "rec" record |> add "y" 50
    Function("rec.Calc", [SimpleValue("y")])
    |> compile data
    |> should equal 100

[<Test>]
let ``Compiles record with function`` () = 
    let record = { Fun = (fun x -> x + 10); NoParam = (fun() -> "Roman") }
    let data = init |> add "rec" record |> add "param" 90
    Function("rec.Fun", [SimpleValue("param")])
    |> compile data
    |> should equal 100

[<Test>]
let ``Compiles record with parameterless function`` () = 
    let record = { Fun = (fun x -> x + 10); NoParam = (fun() -> "Roman") }
    let data = init |> add "rec" record |> add "param" 90
    Function("rec.NoParam", [])
    |> compile data
    |> should equal "Roman"

[<Test>]
let ``Compiles nested record value`` () = 
    let record = { Name = "Roman"; Age = 35; Nested = { Value = 123 } }
    let data = init |> add "rec" record
    SimpleValue("rec.Nested.Value")
    |> compile data
    |> should equal 123

[<Test>]
let ``Compiles tuple function`` () = 
    let tuple = "Roman", 35
    let data = init |> add "tuple" tuple |> add "fst" fst
    Function("fst", [SimpleValue("tuple")])
    |> compile data
    |> should equal "Roman"

[<Test>]
let ``Compiles parameterless function`` () = 
    let fun1 = fun() -> "Roman"
    let data = init |> add "fun1" fun1
    Function("fun1", [])
    |> compile data
    |> should equal "Roman"

[<Test>]
let ``Compiles function with param`` () = 
    let fun1 = fun x -> x + 10
    let data = init |> add "fun1" fun1 |> add "x" 90
    Function("fun1", [SimpleValue("x")])
    |> compile data
    |> should equal 100

[<Test>]
let ``Compiles function with more params`` () = 
    let fun1 = fun x y -> x + y
    let data = init |> add "fun1" fun1 |> add "x" 90 |> add "y" 10
    Function("fun1", [SimpleValue("x"); SimpleValue("y")])
    |> compile data
    |> should equal 100

[<Test>]
let ``Compiles function with nested functions`` () = 
    let addFun = fun x y -> x + y
    let multFun x y = x * y 
    let data = init |> add "add" addFun |> add "mult" multFun |> add "x" 3 |> add "y" 2 |> add "z" 5
    Function("add", [SimpleValue("x"); Function("mult", [SimpleValue("y"); SimpleValue("z")])])
    |> compile data
    |> should equal 13