module Fue.Tests.ValueCompiler

open NUnit.Framework
open FsUnit
open Fue.Core
open Fue.Data
open Fue.ValueCompiler
open Fue.Rop

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

let compileSuccess data = compile data >> extract

[<Test>]
let ``Compiles simple value`` () = 
    let data = init |> add "value" "Roman"
    SimpleValue("value")
    |> compileSuccess data
    |> should equal "Roman"

[<Test>]
let ``Compiles class value`` () = 
    let cls = new Class()
    let data = init |> add "cls" cls
    SimpleValue("cls.Name")
    |> compileSuccess data
    |> should equal "Roman"

[<Test>]
let ``Compiles class method`` () = 
    let cls = new Class()
    let data = init |> add "cls" cls |> add "x" 10
    Function("cls.Y", [SimpleValue("x")])
    |> compileSuccess data
    |> should equal 20

[<Test>]
let ``Compiles class static method`` () = 
    let cls = new Class()
    let data = init |> add "cls" cls |> add "x" 10
    Function("cls.YStatic", [SimpleValue("x")])
    |> compileSuccess data
    |> should equal 20

[<Test>]
let ``Compiles class static value`` () = 
    let cls = new Class()
    let data = init |> add "cls" cls
    SimpleValue("cls.Name_Static")
    |> compileSuccess data
    |> should equal "Roman Static"

[<Test>]
let ``Compiles nested class value`` () = 
    let cls = new Class()
    let data = init |> add "cls" cls
    SimpleValue("cls.Nested.Value")
    |> compileSuccess data
    |> should equal "Nested"

[<Test>]
let ``Compiles record value`` () = 
    let record = { Name = "Roman"; Age = 35; Nested = { Value = 123 } }
    let data = init |> add "rec" record
    SimpleValue("rec.Name")
    |> compileSuccess data
    |> should equal "Roman"

[<Test>]
let ``Compiles record member`` () = 
    let record = { Name = "Roman"; Age = 35; Nested = { Value = 123 } }
    let data = init |> add "rec" record
    SimpleValue("rec.Show")
    |> compileSuccess data
    |> should equal "Roman"

[<Test>]
let ``Compiles record member parameterless method`` () = 
    let record = { Name = "Roman"; Age = 35; Nested = { Value = 123 } }
    let data = init |> add "rec" record
    Function("rec.ShowFunc", [])
    |> compileSuccess data
    |> should equal "Roman"

[<Test>]
let ``Compiles record member method with params`` () = 
    let record = { Name = "Roman"; Age = 35; Nested = { Value = 123 } }
    let data = init |> add "rec" record |> add "y" 50
    Function("rec.Calc", [SimpleValue("y")])
    |> compileSuccess data
    |> should equal 100

[<Test>]
let ``Compiles record with function`` () = 
    let record = { Fun = (fun x -> x + 10); NoParam = (fun() -> "Roman") }
    let data = init |> add "rec" record |> add "param" 90
    Function("rec.Fun", [SimpleValue("param")])
    |> compileSuccess data
    |> should equal 100

[<Test>]
let ``Compiles record with parameterless function`` () = 
    let record = { Fun = (fun x -> x + 10); NoParam = (fun() -> "Roman") }
    let data = init |> add "rec" record |> add "param" 90
    Function("rec.NoParam", [])
    |> compileSuccess data
    |> should equal "Roman"

[<Test>]
let ``Compiles nested record value`` () = 
    let record = { Name = "Roman"; Age = 35; Nested = { Value = 123 } }
    let data = init |> add "rec" record
    SimpleValue("rec.Nested.Value")
    |> compileSuccess data
    |> should equal 123

[<Test>]
let ``Compiles tuple function`` () = 
    let tuple = "Roman", 35
    let data = init |> add "tuple" tuple |> add "fst" fst
    Function("fst", [SimpleValue("tuple")])
    |> compileSuccess data
    |> should equal "Roman"

[<Test>]
let ``Compiles parameterless function`` () = 
    let fun1 = fun() -> "Roman"
    let data = init |> add "fun1" fun1
    Function("fun1", [])
    |> compileSuccess data
    |> should equal "Roman"

[<Test>]
let ``Compiles function with param`` () = 
    let fun1 = fun x -> x + 10
    let data = init |> add "fun1" fun1 |> add "x" 90
    Function("fun1", [SimpleValue("x")])
    |> compileSuccess data
    |> should equal 100

[<Test>]
let ``Compiles function with more params`` () = 
    let fun1 = fun x y -> x + y
    let data = init |> add "fun1" fun1 |> add "x" 90 |> add "y" 10
    Function("fun1", [SimpleValue("x"); SimpleValue("y")])
    |> compileSuccess data
    |> should equal 100

[<Test>]
let ``Compiles function with nested functions`` () = 
    let addFun = fun x y -> x + y
    let multFun x y = x * y 
    let data = init |> add "add" addFun |> add "mult" multFun |> add "x" 3 |> add "y" 2 |> add "z" 5
    Function("add", [SimpleValue("x"); Function("mult", [SimpleValue("y"); SimpleValue("z")])])
    |> compileSuccess data
    |> should equal 13

[<Test>]
let ``Compiles function with generating list`` () = 
    let data = init |> add "fn" (fun _ -> seq {for i in 1..3 do yield i})
    Function("fn", [])
    |> compileSuccess data
    |> should equal [1;2;3]

[<Test>]
let ``Compiles Some and None`` () = 
    let data = init |> add "someValue" (Some "my value")
    SimpleValue("someValue.IsSome")
    |> compileSuccess data
    |> should equal true

[<Test>]
let ``Compiles generic function`` () = 
    let data = init |> add "someFunc" (fun x -> x) |> add "x" 10
    Function("someFunc", [SimpleValue("x")])
    |> compileSuccess data
    |> should equal 10

[<Test>]
let ``Compiles function with constant value`` () = 
    let data = init |> add "print" (fun x -> "printed " + x)
    Function("print", [Constant("hello")])
    |> compileSuccess data
    |> should equal "printed hello"