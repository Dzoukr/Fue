module Fue.Core

type TemplateValue =
    | SimpleValue of name:string
    | Function of name:string * parameters:TemplateValue list

type TemplateNode =
    | ForCycle of item:string * source:TemplateValue
    | IfCondition of source:TemplateValue
    | DiscriminatedUnion of union:TemplateValue * case:string * extract:string list
    | Include of src:string * localData:((string * TemplateValue) list)
