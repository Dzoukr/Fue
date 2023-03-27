open Fake
open Fake.Core
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.Core.TargetOperators
open System.IO
open BuildHelpers
open BuildTools

initializeContext()

let appSrc = "src/Fue"
let testsSrc = "tests/Fue.Tests"

let clean proj = [ proj </> "bin"; proj </> "obj" ] |> Shell.cleanDirs

let createNuget proj =
    clean proj
    run Tools.dotnet "restore --no-cache" proj
    run Tools.dotnet "pack -c Release" proj

let publishNuget proj =
    createNuget proj
    let nugetKey =
        match Environment.environVarOrNone "NUGET_KEY" with
        | Some nugetKey -> nugetKey
        | None -> failwith "The Nuget API key must be set in a NUGET_KEY environmental variable"
    let nupkg =
        Directory.GetFiles(proj </> "bin" </> "Release")
        |> Seq.head
        |> Path.GetFullPath
    run Tools.dotnet (sprintf "nuget push %s -s nuget.org -k %s" nupkg nugetKey) proj

Target.create "Pack" (fun _ ->
    createNuget appSrc
)

Target.create "RunTests" (fun _ ->
    run Tools.dotnet "test" testsSrc
)

Target.create "Publish" (fun _ ->
    publishNuget appSrc
)

let dependencies = [
    "RunTests" ==> "Publish"
]

[<EntryPoint>]
let main args = runOrDefault "BuildApp" args