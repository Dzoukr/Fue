module Fue.Tests.NodeCompiler

open NUnit.Framework
open FsUnit
open Fue.Core
open Fue.Data
open Fue.NodeCompiler
open Fue.Rop
open FSharp.Data

let private toString nodes = nodes |> List.map (fun (x:HtmlNode) -> x.ToString()) |> List.fold (+) ""

let private compileToString data node =
    compile data node
    |> extract
    |> toString

[<Test>]
let ``Compiles if node`` () = 
    let node = HtmlNode.Parse("""<div fs-if="b">{{{value}}}</div>""").[0]
    let data = init |> add "b" true |> add "value" "Roman"
    node 
    |> compileToString data
    |> should equal "<div>Roman</div>"