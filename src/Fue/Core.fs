module Fue.Core

open System
open System.Text.RegularExpressions
open FSharp.Data

let html = 
    """
    <div id="main">
    {{{ message }}}
    {{{ message 2 }}}
    </div>

    <span fs-bind:title="message"></span>

    """

let parse html =
    let regex = new Regex("{{{(.+?)}}}", RegexOptions.Multiline)
    [ for regMatch in regex.Matches(html) do yield regMatch.Groups.[0].Value, regMatch.Groups.[1].Value ]

let h = HtmlDocument.Parse html

let filter (attributes:HtmlAttribute list) =
    attributes |> List.filter (fun x -> x.Name().StartsWith("fs-bind"))

h.Descendants(fun node -> node.Attributes() |> filter |> List.length > 0) |> Seq.toList

parse html
