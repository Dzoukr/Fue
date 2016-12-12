module Fue.NodeCompiler

open Core
open Data
open StringUtils
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

let private toList item = [item]
let private cleanIf (attr:HtmlAttribute) = attr.Name() <> Parser.ifAttr
let private cleanFor (attr:HtmlAttribute) = attr.Name() <> Parser.forAttr
let private cleanDu (attr:HtmlAttribute) = attr.Name() <> Parser.unionSourceAttr && attr.Name() <> Parser.unionCaseAttr
let private cleanNone (attr:HtmlAttribute) = true
let private compileAttributes data = List.map (fun (n,v) -> n, TemplateCompiler.compile data v)
let private prepare data cleanFunc = List.filter cleanFunc >> toTuples >> compileAttributes data

let private foldResults results =
    let foldFn acc item =
        match acc, item with
        | Success(res), Success(list) -> res @ list |> Success
        | Success(_), Failure(errors) -> errors |> Failure
        | Failure(errs), Failure(newErrs) -> errs @ newErrs |> Failure
        | Failure(errs), Success(_) -> errs |> Failure
    results |> List.fold foldFn (Success [])

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
                    let attrs = source.Attributes() |> prepare data cleanIf
                    source.Elements() |> List.map (comp data) |> foldResults
                    >>=> (fun elms ->
                        HtmlNode.NewElement(source.Name(), attrs, elms) |> toList
                    )
                )
            | Some(ForCycle(item, cycle)) ->
                cycle |> ValueCompiler.compile data
                >>= checkIsIterable
                >>= (fun list ->
                    list |> Seq.map (fun l ->
                        let attrs = source.Attributes() |> prepare data cleanFor
                        let dataWithItem = data |> add item l
                        source.Elements() |> List.map (comp dataWithItem) |> foldResults
                        >>=> (fun elms ->
                            HtmlNode.NewElement(source.Name(), attrs, elms) |> toList
                        )
                    ) |> Seq.toList |> foldResults
                )
            | Some(DiscriminatedUnion(union,case,extracts)) ->
                union |> ValueCompiler.compile data
                >>= extractCase case extracts
                >>= (fun (isMatch, dataToAdd) ->
                    if isMatch then
                        let attrs = source.Attributes() |> prepare data cleanDu
                        let newData = data |> addMany dataToAdd
                        source.Elements() |> List.map (comp newData) |> foldResults
                        >>=> (fun elms ->
                            HtmlNode.NewElement(source.Name(), attrs, elms) |> toList
                        )
                    else
                        [] |> success
                )
            | None ->
                match source.Name() with
                | "" -> 
                    source.DirectInnerText() 
                    |> TemplateCompiler.compile data
                    |> HtmlNode.NewText
                    |> toList |> success
                | name ->
                    source.Elements() |> List.map (comp data) |> foldResults
                    >>=> (fun elms ->
                        let attrs = source.Attributes() |> prepare data cleanNone
                        HtmlNode.NewElement(name, attrs, elms) |> toList
                    )
        )
    comp data source