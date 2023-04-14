module Fue.Core

type TemplateValue =
    | SimpleValue of name:string
    | Function of name:string * parameters:TemplateValue list
    | Literal of value:obj
    | Record of Map<string, TemplateValue>
    | NullCoalesce of TemplateValue * TemplateValue

type TemplateNode =
    | ForCycle of item:string * source:TemplateValue
    | ConditionalForCycle of item: string * source:TemplateValue * condition: TemplateValue
    | IfCondition of source:TemplateValue
    | ElseCondition
    | DiscriminatedUnion of union:TemplateValue * case:string * extract:string list
