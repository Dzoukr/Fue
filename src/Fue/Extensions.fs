module internal Fue.Extensions

open HtmlAgilityPack

type HtmlNode with
    member this.TryGetAttribute attr = this.Attributes |> Seq.filter (fun x -> x.Name = attr) |> Seq.tryHead

type HtmlDocument with
    static member Create() =
        let doc = new HtmlDocument()
        doc.OptionOutputOriginalCase <- true
        doc

    static member ParseNode html =
        let doc = HtmlDocument.Create()
        // fix for http://stackoverflow.com/questions/293342/htmlagilitypack-drops-option-end-tags
        HtmlNode.ElementsFlags.Remove("option") |> ignore
        HtmlNode.ElementsFlags.Remove("param") |> ignore
        HtmlNode.ElementsFlags.Remove("link") |> ignore
        doc.LoadHtml(html)
        doc.DocumentNode
