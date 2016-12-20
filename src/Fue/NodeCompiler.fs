module internal Fue.NodeCompiler

open Core
open Data
open Rop
open HtmlAgilityPack
open Microsoft.FSharp.Reflection
open Extensions

let private checkIsBool (value:obj) = 
    if(value :? bool) then value :?> bool |> success 
    else ValueExpectedToBeBoolean(value) |> fail

let private checkIsIterable (value:obj) = 
    if(value :? System.Collections.IEnumerable) then 
        seq { for v in value :?> System.Collections.IEnumerable do yield v } |> success 
    else ValueExpectedToBeIterable(value) |> fail

let private checkExtractsLength (extracts:string list) (values:obj []) =
    match extracts.Length, values.Length with
    | x, y when x >= y -> (extracts, values) |> success 
    | x, y -> ListOfDUExtractionIsLongerThanCaseValues(x, y) |> fail
        
let private asResults item = [item] |> success
let private cleanIf (attr:HtmlAttribute) = attr.Name <> Parser.ifAttr
let private cleanIfNot (attr:HtmlAttribute) = attr.Name <> Parser.ifNotAttr
let private cleanFor (attr:HtmlAttribute) = attr.Name <> Parser.forAttr
let private cleanDu (attr:HtmlAttribute) = attr.Name <> Parser.unionSourceAttr && attr.Name <> Parser.unionCaseAttr
let private cleanNone (attr:HtmlAttribute) = true

let private compileAttributes data attrs = 
    attrs 
    |> Seq.map (fun (a:HtmlAttribute) -> TemplateCompiler.compile data a.Value >>=> (fun res -> [a.OriginalName, res]))
    |> Seq.toList 
    |> Rop.fold

let private prepareAttributes data cleanFunc = Seq.filter cleanFunc >> compileAttributes data

let private newElements name children attributes = 
    let node = HtmlDocument().CreateElement(name)
    attributes |> Seq.iter (fun (name, value) -> node.SetAttributeValue(name, value) |> ignore)
    children |> List.iter (fun x -> if x <> null then node.AppendChild(x) |> ignore else ())
    node |> asResults

let private extractCase case (extracts:string list) union =
    let info, values = FSharpValue.GetUnionFields(union, union.GetType())
    if info.Name <> case then 
        (false, []) |> success
    else 
        checkExtractsLength extracts values
        >>=> (fun (ex,vals) ->
            (true,[ for i in [0..vals.Length - 1] do yield ex.[i], values.[i] ])
        )

let private compileIf compileFun (source:HtmlNode) data boolFun cleanFun boolValue =
    boolValue |> ValueCompiler.compile data
    >>= checkIsBool
    >>= (fun bValue ->
        if boolFun(bValue) then
            let attrs = source.Attributes |> prepareAttributes data cleanFun
            let elms = source.ChildNodes |> Seq.toList |> List.map (compileFun data) |> Rop.fold
            Rop.bind2 (newElements source.Name) elms attrs
        else
            [] |> success
    )

let private compileForCycle compileFun (source:HtmlNode) data itemName cycle =
    cycle |> ValueCompiler.compile data
    >>= checkIsIterable
    >>= (fun list ->
        let length = Seq.length list
        list |> Seq.map (fun itemValue ->
            let index = (list |> Seq.findIndex (fun x -> x = itemValue))
            let dataWithItem = 
                data 
                |> add itemName itemValue 
                |> add "$index" index
                |> add "$iteration" (index + 1)
                |> add "$length" length
            let attrs = source.Attributes |> prepareAttributes dataWithItem cleanFor
            let elms = source.ChildNodes |> Seq.toList |> List.map (compileFun dataWithItem) |> Rop.fold
            Rop.bind2 (newElements source.Name) elms attrs
        ) |> Seq.toList |> Rop.fold
    )

let private compileUnion compileFun (source:HtmlNode) data union case extracts =
    union |> ValueCompiler.compile data
    >>= extractCase case extracts
    >>= (fun (isMatch, dataToAdd) ->
        if isMatch then
            let attrs = source.Attributes |> prepareAttributes data cleanDu
            let newData = data |> addMany (dataToAdd |> List.filter (fun x -> fst x <> "_"))
            let elms = source.ChildNodes |> Seq.toList |> List.map (compileFun newData) |> Rop.fold
            Rop.bind2 (newElements source.Name) elms attrs
        else
            [] |> success
    )

let private compileOther compileFun (source:HtmlNode) data =
    let elms = source.ChildNodes |> Seq.toList |> List.map (compileFun data) |> Rop.fold
    let attrs = source.Attributes |> prepareAttributes data cleanNone
    Rop.bind2 (newElements source.Name) elms attrs

/// Applies attributes/interpolation logic onto Html node tree
let compile data (source:HtmlNode) =
    let rec comp data source =
        
        let compileIf = compileIf comp source data
        let compileForCycle = compileForCycle comp source data
        let compileUnion = compileUnion comp source data

        Parser.parseNode(source)
        >>= (fun node ->
            match node with
            | Some(IfCondition(boolValue)) -> boolValue |> compileIf (fun x -> x = true) cleanIf
            | Some(IfNotCondition(boolValue)) -> boolValue |> compileIf (fun x -> x = false) cleanIfNot
            | Some(ForCycle(itemName, cycle)) -> compileForCycle itemName cycle
            | Some(DiscriminatedUnion(union,case,extracts)) -> compileUnion union case extracts
            | None ->
                match source.Name with
                | "#text" | "#comment" -> source.InnerHtml |> TemplateCompiler.compile data >>=> HtmlDocument.ParseNode >>= asResults
                | _ -> compileOther comp source data
        )
    comp data source