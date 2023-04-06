[![Build Status](https://dev.azure.com/dzoukrcz/Fue/_apis/build/status/Fue?branchName=master)](https://dev.azure.com/dzoukrcz/Fue/_build/latest?definitionId=2&branchName=master)

# Fue

F# templating library with simple syntax designed for smooth work with F# types.

## Why another templating library?

We have [Razor](https://github.com/Antaris/RazorEngine), we have [DotLiquid](http://dotliquidmarkup.org/) - why another templating library? I know, "rendering on server side is so 2010", but sometimes we just need (or want) to do it - for emails, for documents, even for HTML (yes, some oldschoolers still do it on server). And then pain starts: You need to have plenty of *ViewModels* to transform data from Discriminated Unions, Tuples, etc... 

Wouldn\`t be just great to have a library that allows you to use your original data without annoying only-for-template transformation? Good news! Fue was designed as *ViewModels |> NoMore* library with focus on minimalistic API.


## Installation
First install NuGet package

    Install-Package Fue

or using [Paket](http://fsprojects.github.io/Paket/getting-started.html)

    nuget Fue


## Basic templating

Before we start, open these two modules:

```fsharp
open Fue.Data
open Fue.Compiler
```

Now we need storage for our view data. Function `init` from `Fue.Data` module is here for you. Once initiated, you can store values in it.

```fsharp
init |> add "name" "Roman"
```

Having data prepared, lets start rendering our values using simple `{{{myValue}}}` syntax.

```fsharp
init |> add "name" "Roman" |> fromText "{{{name}}}" // compiles to value "Roman"
```

Full example:

```fsharp
let html = "<div>{{{name}}}</div>"
let compiledHtml = init |> add "name" "Roman" |> fromText html
// compiledHtml now contains "<div>Roman</div>"
```

If a variable is not defined, it gets rendered as is - including the braces.

```fsharp
let html = "<div>{{{name}}}</div>"
let compiledHtml = init |> fromText html
// compiledHtml now contains "<div>{{{name}}}</div>"
```

However, if you want to have a default value in case the variable is not defined, you can do that with `??`:
```fsharp
let html = "<div>{{{name ?? "Roman"}}}</div>"
let compiledHtml = init |> fromText html
// compiledHtml now contains "<div>Roman</div>"
```

Wanna use functions? No problem!

```fsharp
let html = "<div>{{{getName()}}}</div>"
let compiledHtml = init |> add "getName" (fun _ -> "Roman") |> fromText html
// compiledHtml now contains "<div>Roman</div>"
```

Or pipe forward operator?

```fsharp
let html = "<div>{{{myParam |> multiply}}}</div>"
let compiledHtml =
    init
    |> add "myParam" 10
    |> add "multiply" (fun x -> x * 2)
    |> fromText html
// compiledHtml now contains "<div>20</div>"
```

And combine own functions with literals.

```fsharp
let html = """<div>{{{printHello("Roman", myValue)}}}</div>"""
let compiledHtml =
    init
    |> add "printHello" (fun name1 name2 -> sprintf "Hello %s and %s" name1 name2)
    |> add "myValue" "Jiri"
    |> fromText html
// compiledHtml now contains "<div>Hello Roman and Jiri</div>"
```

You can also pass records to functions.

```fsharp
let html = """<div>{{{printHello({ name = "Roman" })}}}</div>"""
let compiledHtml =
    init
    |> add "printHello" (fun name1 name2 -> sprintf "Hello %s and %s" name1 name2)
    |> add "myValue" "Jiri"
    |> fromText html
// compiledHtml now contains "<div>Hello Roman and Jiri</div>"
```

**Please note:** For better work with HTML templates, literals syntax can be marked with both 'single quotes' or "double quotes" and """tripple quotes""" if you need multiline text

Record values can contain any other value including records.

```fsharp
{
    a: "bar" // literal as value
    b: abc   // variable as value
    c: abc() // function call
    d: "abc" |> abc // pipes
    c: {     // nested record
       a: "bar" 
       ....
    }
}
```



## Supported types

Fue is designed to work with *classes, records, tuples, options, discriminated unions as well as anonymous functions*.

```fsharp
type MyRecord = { Name : string }
let html = """<div id="{{{id}}}">{{{fst myTuple}}} {{{myRec.Name}}}</div>"""
let compiledHtml =
    init
    |> add "myTuple" ("Hello", 35)
    |> add "fst" fst
    |> add "myRec" { Name = "John"}
    |> add "id" "someId"
    |> fromText html
// compiledHtml now contains """<div id="someId">Hello John</div>"""
```

## Rendering from file

Common usage of template engines is to have templates separated as files. Function `fromFile` is here for you.

```fsharp
let compiledHtml =
    init
    |> add "someValue" "myValue"
    |> fromFile "relative/or/absolute/path/to/file.html"
```

## Conditional rendering

True power of Fue library is in custom attributes which can affect how will be template rendered.

### fs-if

Simple *if condition* attribute.

```fsharp
let html = """<div fs-if="render">This DIV won`t be rendered at all</div>"""
let compiledHtml =
    init
    |> add "render" false
    |> fromText html
// compiledHtml is empty string
```

### fs-else

Simple *else condition* attribute.

```fsharp
let html = """<div fs-if="render">Not rendered</div><div fs-else>This will be rendered</div>"""
let compiledHtml =
    init
    |> add "render" false
    |> fromText html
// compiledHtml is "<div>This will be rendered</div>"
```

**Please note**: Else condition must immediately follow If condition.

### fs-du & fs-case

Condition based on value of *Discriminated Union*.

```fsharp
type Access =
    | Anonymous
    | Admin of username:string

let html =
    """
    <div fs-du="access" fs-case="Anonymous">Welcome unknown!</div>
    <div fs-du="access" fs-case="Admin(user)">Welcome {{{user}}}</div>
    """
let compiledHtml =
    init
    |> add "access" (Access.Admin("Mr. Boosh"))
    |> fromText html
// compiledHtml is "<div>Welcome Mr. Boosh</div>"
```

Of course, if you do not need associated case values, you can ignore them.

```fsharp
let html =
    """
    <div fs-du="access" fs-case="Anonymous">Welcome unknown!</div>
    <div fs-du="access" fs-case="Admin(_)">Welcome admin</div>
    """
let compiledHtml =
    init
    |> add "access" (Access.Admin("Mr. Boosh"))
    |> fromText html
// compiledHtml is "<div>Welcome admin</div>"
```

Fue syntax allows you to do not extract any value (even if there is some associated).

```fsharp
let html =
    """
    <div fs-du="access" fs-case="Anonymous">Welcome unknown!</div>
    <div fs-du="access" fs-case="Admin">Welcome admin</div>
    """
```

### fs-template

Non-rendered placeholder for text. Use with combination of other Fue attributes.

```fsharp
let html = """<fs-template fs-if="render">Rendered only inner Html</fs-template>"""
let compiledHtml =
    init
    |> add "render" true
    |> fromText html
// compiledHtml is "Rendered only inner Html"
```

## List rendering

### fs-for

*For-cycle* attribute

```fsharp
let html = """<ul><li fs-for="item in items">{{{item}}}</li></ul>"""
let compiledHtml =
    init
    |> add "items" ["A";"B";"C"]
    |> fromText html
// compiledHtml is "<ul><li>A</li><li>B</li><li>C</li></ul>"
```

Common task for rendering lists is to show row number, index or whole list length. Auto-created values `{{{$index}}}`, `{{{$iteration}}}`, `{{{$length}}}`, `{{{$is-last}}}` and `{{{$is-not-last}}}` are here to help.

```fsharp
let html = """<li fs-for="i in items">{{{i}}} is {{{$index}}}, {{{$iteration}}}, {{{$length}}}</li>"""
let compiledHtml =
    init
    |> add "items" ["A";"B";"C"]
    |> fromText html
// compiledHtml is "<li>A is 0, 1, 3</li><li>B is 1, 2, 3</li><li>C is 2, 3, 3</li>"
```

Since version 1.2.0 there is support for tuples destructuring:

```fsharp
let html = """<li fs-for="greetings,target in items">I say {{{greetings}}} to {{{target}}}</li>"""
let compiledHtml =
    init
    |> add "items" [("Hi","World");("Hello","Planet")]
    |> fromText html
// compiledHtml is "<li>I say Hi to World</li><li>I say Hello to Planet</li>"
```


## Working with Option types

*Option* types are fully supported and you can use them as you would directly from F# code.

```fsharp
let html = """<div fs-if="myOptionValue.IsSome">I got {{{myOptionValue.Value}}}</div>"""
let compiledHtml =
    init
    |> add "myOptionValue" (Some "abc")
    |> fromText html
// compiledHtml is "<div>I got abc</div>"
```

or

```fsharp
let html = """<div fs-if="myOptionValue.IsNone">I got nothing</div>"""
let compiledHtml =
    init
    |> add "myOptionValue" None
    |> fromText html
// compiledHtml is "<div>I got nothing</div>"
```

## Cheatsheet

Simple HTML snippet to show what can be achieved using Fue:

```html
<!--Literals-->
{{{'single quoted literal'}}}
{{{"single line literal"}}}
{{{"""multi-line
literal"""}}}

<!--Template basics-->
{{{value}}} - Static value
{{{value ?? "default value"}}} - Default Value
{{{function()}}} - Function value
{{{value1 |> fun1}}}
{{{ { key = value; key2 = value2 } }}}
{{{ {
    key = value
    key2 = value2
} }}}

<!--For-cycle-->
<li fs-for="item in items">
    {{{item.Name}}} {{{$index}}} {{{$length}}} {{{$iteration}}}
    <div fs-if="$is-not-last">Shown if not last item of collection</div>
    <div fs-if="$is-last">Shown only for the last item of collection</div>
</li>

<!--For-cycle with direct tuple destructuring-->
<!--let items = [("Hi","World");("Hello";"Planet")]-->
<li fs-for="greetings,target in items">
    I say {{{greetings}}} to {{{target}}}
</li>

<!--Condition-->
<div fs-if="someCondition" id="{{{id}}}">Value</div>
<div fs-else></div>

<!--Option types-->
<div fs-if="someOption.IsSome">{{{someOption.Value}}}</div>
<div fs-if="someOption.IsNone">Nothing</div>

<!--Discriminated Union
type UserAccess =
    | Anonymous
    | Admin of section:string
-->
    <div fs-du="item" fs-case="Anonymous">Anonymous</div>
    <div fs-du="item" fs-case="Admin(section)">{{{section}}}</div>

<!--Placeholder-->
<fs-template fs-if="someCondition">
    Some value
</fs-template>
```

## Used libraries

Fue is based on amazing [Html Agility Pack](http://htmlagilitypack.codeplex.com/) library.

## Contribution
Did you find any bug? Missing functionality? Please feel free to [Create issue](https://github.com/Dzoukr/Fue/issues) or [Pull request](https://github.com/Dzoukr/Fue/pulls).
