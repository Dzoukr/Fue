module Fue.NodeCompiler

open Core
open Data
open Rop
open FSharp.Data
open Microsoft.FSharp.Reflection

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
        
let private bindIf f t b  = if b then t() else (f |> success)
let private toTuples = List.map (fun (x:HtmlAttribute) -> x.Name(), x.Value())

let private asResults item = [item] |> success
let private cleanIf (attr:HtmlAttribute) = attr.Name() <> Parser.ifAttr
let private cleanFor (attr:HtmlAttribute) = attr.Name() <> Parser.forAttr
let private cleanDu (attr:HtmlAttribute) = attr.Name() <> Parser.unionSourceAttr && attr.Name() <> Parser.unionCaseAttr
let private cleanNone (attr:HtmlAttribute) = true

let private compileAttributes data list = 
    list 
    |> List.map (fun (k,v) -> TemplateCompiler.compile data v >>=> (fun res -> [k, res])) 
    |> Rop.fold

let private prepareAttributes data cleanFunc = List.filter cleanFunc >> toTuples >> compileAttributes data

let extractCase case (extracts:string list) union =
    let info, values = FSharpValue.GetUnionFields(union, union.GetType())
    if info.Name <> case then 
        (false, []) |> success
    else 
        checkExtractsLength extracts values
        >>=> (fun (ex,vals) ->
            (true,[ for i in [0..vals.Length - 1] do yield ex.[i], values.[i] ])
        )

/// Applies attributes/interpolation logic onto Html node tree
let compile data (source:HtmlNode) =
    let rec comp data source  =
        Parser.parseNode(source)
        >>= (fun node ->
            match node with
            | Some(IfCondition(boolValue)) ->
                boolValue |> ValueCompiler.compile data
                >>= checkIsBool
                >>= bindIf [] (fun _ ->
                    let attrs = source.Attributes() |> prepareAttributes data cleanIf
                    let elms = source.Elements() |> List.map (comp data) |> Rop.fold
                    Rop.bind2 (fun elms attrs ->
                        HtmlNode.NewElement(source.Name(), attrs, elms) |> asResults
                    ) elms attrs
                )
            | Some(ForCycle(itemName, cycle)) ->
                cycle |> ValueCompiler.compile data
                >>= checkIsIterable
                >>= (fun list ->
                    list |> Seq.map (fun itemValue ->
                        let dataWithItem = data |> add itemName itemValue
                        let attrs = source.Attributes() |> prepareAttributes dataWithItem cleanFor
                        let elms = source.Elements() |> List.map (comp dataWithItem) |> Rop.fold
                        Rop.bind2 (fun elms attrs ->
                            HtmlNode.NewElement(source.Name(), attrs, elms) |> asResults
                        ) elms attrs
                    ) |> Seq.toList |> Rop.fold
                )
            | Some(DiscriminatedUnion(union,case,extracts)) ->
                union |> ValueCompiler.compile data
                >>= extractCase case extracts
                >>= (fun (isMatch, dataToAdd) ->
                    if isMatch then
                        let attrs = source.Attributes() |> prepareAttributes data cleanDu
                        let newData = data |> addMany dataToAdd
                        let elms = source.Elements() |> List.map (comp newData) |> Rop.fold
                        Rop.bind2 (fun elms attrs ->
                            HtmlNode.NewElement(source.Name(), attrs, elms) |> asResults
                        ) elms attrs
                    else
                        [] |> success
                )
            | None ->
                match source.Name() with
                | "" -> 
                    source.DirectInnerText() 
                    |> TemplateCompiler.compile data
                    >>=> HtmlNode.NewText 
                    >>= asResults
                | name ->
                    let elms = source.Elements() |> List.map (comp data) |> Rop.fold
                    let attrs = source.Attributes() |> prepareAttributes data cleanNone
                    Rop.bind2 (fun elms attrs -> 
                        HtmlNode.NewElement(name, attrs, elms) |> asResults
                    ) elms attrs
        )
    comp data source