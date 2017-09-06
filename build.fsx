// include Fake lib
#I "packages/FAKE/tools/"

#r "FakeLib.dll"

open System
open System.IO
open Fake 
open Fake.AssemblyInfoFile
open System.Diagnostics
open Fake.Testing

let appSrc = "src/Fue"
let testsSrc = "tests/Fue.Tests"

// Read release notes & version info from RELEASE_NOTES.md
let release = File.ReadLines "RELEASE_NOTES.md" |> ReleaseNotesHelper.parseReleaseNotes

Target "Nuget" <| fun () ->
    let toNotes = List.map (fun x -> x + System.Environment.NewLine) >> List.fold (+) ""
    let args = 
        [
            "PackageId=\"Fue\""
            "Title=\"Fue\""
            "Description=\"F# templating library with simple syntax designed for smooth work with F# types\""
            "Summary=\"F# templating library with simple syntax designed for smooth work with F# types\""
            sprintf "PackageVersion=\"%s\"" release.NugetVersion
            sprintf "PackageReleaseNotes=\"%s\"" (release.Notes |> toNotes)
            "PackageLicenseUrl=\"http://github.com/dzoukr/Fue/blob/master/LICENSE.md\""
            "PackageProjectUrl=\"http://github.com/dzoukr/Fue\""
            "PackageIconUrl=\"https://avatars2.githubusercontent.com/u/851307?v=3&amp;s=64\""
            "PackageTags=\"FSharp Templating F# Templates\""
            "Copyright=\"Roman Provazník - 2017\""
            "Authors=\"Roman Provazník\""
        ] |> List.map (fun x -> "/p:" + x)

    Fake.DotNetCli.Pack (fun p -> { p with Project = appSrc; OutputPath = "../../nuget"; AdditionalArgs = args })

Target "BuildApp" (fun _ ->
    Fake.DotNetCli.Build (fun p -> { p with Project = appSrc; Configuration = "Debug";})
)

Target "RunTests" (fun _ ->
     Fake.DotNetCli.Test (fun p -> { p with Project = testsSrc; Configuration = "Debug";})
)

Target "Clean" (fun _ -> 
    DeleteDir (appSrc + "/bin")
    DeleteDir (appSrc + "/obj")
    DeleteDir "nuget" 
)

"Clean" ==> "RunTests" ==>  "Nuget"

// start build
RunTargetOrDefault "BuildApp"