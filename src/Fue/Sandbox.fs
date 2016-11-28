module Fue.Sandbox

open System
open System.Text.RegularExpressions
open FSharp.Data


let html = 
    """
    <div id="main">
    {{{ message }}}
    {{{ message 2 }}}
        <fs-include fs-src="" fs-data="">
    </div>

    <span fs-bind:title="message"></span>

    """

let html2 =
    """
    <div id="a">
        {{{A}}}
        <div id="a1">
            {{{A1}}}
            <div id="a1b">
            A1B
            </div>
        </div>
        AA
        <div id="a2">
        A2
        </div>
    </div>
    """

//let vals = "<x>{{{X}}} TOHLE JE NIC {{{Y}}}</x>" |> HtmlDocument.Parse
let vals = html2 |> HtmlDocument.Parse
vals.Descendants() |> Seq.toList |> List.map (fun x -> x.DirectInnerText())

let parse html =
    let regex = new Regex("{{{(.+?)}}}", RegexOptions.Multiline)
    [ for regMatch in regex.Matches(html) do yield regMatch.Groups.[0].Value, regMatch.Groups.[1].Value ]

let h = HtmlDocument.Parse html

let filter (attributes:HtmlAttribute list) =
    attributes |> List.filter (fun x -> x.Name().StartsWith("fs-bind"))

h.Descendants(fun node -> node.Attributes() |> filter |> List.length > 0) |> Seq.toList

parse html

h.Descendants() |> Seq.toList |> List.map (fun x -> x.Descendants() |> Seq.toList)

