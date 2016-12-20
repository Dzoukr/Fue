module internal Fue.Extensions

open HtmlAgilityPack

type HtmlNode with
    member this.TryGetAttribute attr = this.Attributes |> Seq.filter (fun x -> x.Name = attr) |> Seq.tryHead

type HtmlDocument with
    static member ParseNode html =
        let doc = new HtmlDocument()
        doc.LoadHtml(html)
        doc.DocumentNode