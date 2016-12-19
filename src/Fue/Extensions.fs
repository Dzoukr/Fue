module Fue.Extensions

open HtmlAgilityPack

type HtmlNode with
    member this.TryGetAttribute attr = this.Attributes |> Seq.filter (fun x -> x.Name = attr) |> Seq.tryHead