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
let ``Compiles true if`` () = 
    let node = HtmlNode.Parse("""<div fs-if="b">{{{value}}}</div>""").[0]
    let data = init |> add "b" true |> add "value" "Roman"
    node 
    |> compileToString data
    |> should equal "<div>Roman</div>"

[<Test>]
let ``Compiles false if`` () = 
    let node = HtmlNode.Parse("""<div fs-if="b">{{{value}}}</div>""").[0]
    let data = init |> add "b" false
    node 
    |> compileToString data
    |> should equal ""


[<Test>]
let ``Compiles for from list`` () = 
    let node = HtmlNode.Parse("""<div fs-for="x in y">{{{x}}}</div>""").[0]
    let data = init |> add "y" ["a";"b";"c"]
    node 
    |> compileToString data
    |> should equal "<div>a</div><div>b</div><div>c</div>"

[<Test>]
let ``Compiles for from array`` () = 
    let node = HtmlNode.Parse("""<div fs-for="x in y">{{{x}}}</div>""").[0]
    let data = init |> add "y" [|"a";"b";"c"|]
    node 
    |> compileToString data
    |> should equal "<div>a</div><div>b</div><div>c</div>"

[<Test>]
let ``Compiles for from seq function`` () = 
    let node = HtmlNode.Parse("""<div fs-for="x in y()">{{{x}}}</div>""").[0]
    let data = init |> add "y" (fun () -> seq { for i in [1..3] do yield i } |> Seq.toList)
    node 
    |> compileToString data
    |> should equal "<div>1</div><div>2</div><div>3</div>"

type MyUnion =
    | Admin
    | User of string * int

[<Test>]
let ``Extracts discriminated union cases`` () = 
    MyUnion.User("ahoj", 42)
    |> extractCase "User" ["a";"b"] 
    |> extract
    |> should equal (true, [("a","ahoj" |> box);("b",42 |> box)])

[<Test>]
let ``Compiles discriminated union`` () = 
    let node = HtmlNode.Parse("""<div id="82" fs-du="src" fs-case="User(x,y)">{{{y}}},{{{x}}}</div>""").[0]
    let data = init |> add "src" (MyUnion.User("Roman",42))
    node 
    |> compileToString data
    |> should equal """<div id="82">42,Roman</div>"""

[<Test>]
let ``Ignores discriminated union with different case`` () = 
    let node = HtmlNode.Parse("""START<div fs-du="src" fs-case="User(x,y)">{{{y}}},{{{x}}}</div>""").[0]
    let data = init |> add "src" (MyUnion.Admin)
    node 
    |> compileToString data
    |> should equal "START"


[<Test>]
let ``Compiles attribute values`` () = 
    let node = HtmlNode.Parse("""<div id="{{{who}}}">{{{me}}}</div>""").[0]
    let data = init |> add "who" "Roman" |> add "me" "Dzoukr"
    node 
    |> compileToString data
    |> should equal """<div id="Roman">Dzoukr</div>"""