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

type RecordWithFun = {
    Fun : int -> int
}

[<Test>]
let ``Compiles simple value`` () = 
    let data = init |> add "value" "Roman"
    SimpleValue("value")
    |> compile data
    |> should equal "Roman"

[<Test>]
let ``Compiles record value`` () = 
    let record = { Name = "Roman"; Age = 35; Nested = { Value = 123 } }
    let data = init |> add "rec" record
    SimpleValue("rec.Name")
    |> compile data
    |> should equal "Roman"

[<Test>]
let ``Compiles record with function`` () = 
    let record = { Fun = (fun x -> x + 10) }
    let data = init |> add "rec" record |> add "param" 90
    Function("rec.Fun", [SimpleValue("param")])
    |> compile data
    |> should equal 100

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