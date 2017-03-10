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

let private prepareExtracts case (extracts:string list) (values:obj []) =
    match extracts.Length, values.Length with
    | 0, _ -> (extracts, values) |> success
    | x, y when x = y -> (extracts, values) |> success 
    | x, y -> ListOfDUExtractionHasDifferentLength(case, x, y) |> fail
        
let private asResults item = [item] |> success
let private filterIf (attr:HtmlAttribute) = attr.Name <> Parser.ifAttr
let private filterElse (attr:HtmlAttribute) = attr.Name <> Parser.elseAttr
let private filterFor (attr:HtmlAttribute) = attr.Name <> Parser.forAttr
let private filterDu (attr:HtmlAttribute) = attr.Name <> Parser.unionSourceAttr && attr.Name <> Parser.unionCaseAttr
let private filterNone (attr:HtmlAttribute) = true

let private compileAttributes data attrs = 
    attrs 
    |> Seq.map (fun (a:HtmlAttribute) -> TemplateCompiler.compile data a.Value >>=> (fun res -> [a.OriginalName, res]))
    |> Seq.toList 
    |> Rop.fold

let private prepareAttributes data cleanFunc = Seq.filter cleanFunc >> compileAttributes data

let private newElements name children attributes = 
    let node = HtmlDocument.Create().CreateElement(name)
    attributes |> Seq.iter (fun (name, value) -> node.SetAttributeValue(name, value) |> ignore)
    children |> List.iter (fun x -> if x <> null then node.AppendChild(x) |> ignore else ())
    node |> asResults

let private extractCase case (extracts:string list) union =
    let info, values = FSharpValue.GetUnionFields(union, union.GetType())
    if info.Name <> case then 
        (false, []) |> success
    else 
        prepareExtracts case extracts values
        >>=> (fun (ex,_) ->
            (true,[ for i in [0..ex.Length - 1] do yield ex.[i], values.[i] ])
        )

let private compileNode compileFun (source:HtmlNode) data attributesFilter =
    let elms = source.ChildNodes |> Seq.toList |> List.map (compileFun data) |> Rop.fold
    match source.Name with
    | "fs-template" -> elms
    | _ ->
        let attrs = source.Attributes |> prepareAttributes data attributesFilter
        Rop.bind2 (newElements source.OriginalName) elms attrs

let private compileIf compileFun (source:HtmlNode) data boolValue =
    boolValue |> ValueCompiler.compile data
    >>= checkIsBool
    >>= (fun bValue -> if bValue then compileNode compileFun source data filterIf else [] |> success)

let private compileElse compileFun (source:HtmlNode) data boolValue =
    boolValue |> ValueCompiler.compile data
    >>= checkIsBool
    >>= (fun bValue -> if bValue = false then compileNode compileFun source data filterElse else [] |> success)

let private getDestructuredFields =
    StringUtils.split ',' 
    >> List.map (StringUtils.clean >> StringUtils.replace "(" "" >> StringUtils.replace ")" "")

let private areMappable list1 list2 = List.length list1 = List.length list2

let private mapOneToOne (fields:string list) values data =
    List.zip fields values
    |> List.fold (fun acc item -> acc |> add (fst item) (snd item)) data

let private addForCycleItem name value data =
    match value |> Reflection.isTuple with
    | true ->
        let fields = getDestructuredFields name
        let tupleValues = Reflection.getTupleFields value
        
        if areMappable fields tupleValues then
            data |> mapOneToOne fields tupleValues
        else
            data |> add name value
    | false -> data |> add name value
    
let private compileForCycle compileFun (source:HtmlNode) data itemName cycle =
    cycle |> ValueCompiler.compile data
    >>= checkIsIterable
    >>= (fun list ->
        let length = Seq.length list
        list |> Seq.map (fun itemValue ->
            let index = (list |> Seq.findIndex (fun x -> x = itemValue))
            let dataWithItem = 
                data 
                //|> add itemName itemValue 
                |> addForCycleItem itemName itemValue
                |> add "$index" index
                |> add "$iteration" (index + 1)
                |> add "$length" length
            compileNode compileFun source dataWithItem filterFor
        ) |> Seq.toList |> Rop.fold
    )

let private compileUnion compileFun (source:HtmlNode) data union case extracts =
    union |> ValueCompiler.compile data
    >>= extractCase case extracts
    >>= (fun (isMatch, dataToAdd) ->
        if isMatch then
            let newData = data |> addMany (dataToAdd |> List.filter (fun x -> fst x <> "_"))
            compileNode compileFun source newData filterDu
        else
            [] |> success
    )

let rec private findNonEmptyPreviousSibling (source:HtmlNode) =
    let sibling = source.PreviousSibling
    match sibling with
    | null -> None
    | sibling -> if sibling.OuterHtml |> StringUtils.isWhiteSpace then findNonEmptyPreviousSibling sibling else Some sibling

/// Applies attributes/interpolation logic onto Html node tree
let compile data (source:HtmlNode) =
    let rec comp data source =
        
        let compileIf = compileIf comp source data
        let compileElse = compileElse comp source data
        let compileForCycle = compileForCycle comp source data
        let compileUnion = compileUnion comp source data

        Parser.parseNode(source)
        >=>> (fun node ->
            match node with
            | Some(IfCondition(boolValue)) -> boolValue |> compileIf
            | Some(ElseCondition) -> 
                source |> findNonEmptyPreviousSibling 
                >=>> failForNone (ElseConditionMustImmediatelyFollowIfCondition)
                >>=> Parser.parseNode
                >>= (fun n ->
                    match n with
                    | Some(IfCondition(boolValue)) -> boolValue |> compileElse
                    | _ -> ElseConditionMustImmediatelyFollowIfCondition |> fail
                )
            | Some(ForCycle(itemName, cycle)) -> compileForCycle itemName cycle
            | Some(DiscriminatedUnion(union,case,extracts)) -> compileUnion union case extracts
            | None ->
                match source.Name with
                | "#text" | "#comment" -> source.InnerHtml |> TemplateCompiler.compile data >>=> HtmlDocument.ParseNode >>= asResults
                | _ -> compileNode comp source data filterNone
        )
    comp data source