// include Fake lib
#I "packages/FAKE/tools/"

#r "FakeLib.dll"

open System
open System.IO
open Fake 
open Fake.AssemblyInfoFile
open System.Diagnostics
open Fake.Testing

let title = "Fue" // OK

let appBuildDir = "./build/app/"
let appSrcDir = "./src/"
let testsBuildDir = "./build/tests/"
let testsSrcDir = "./tests"

let description = ""

// Read release notes & version info from RELEASE_NOTES.md
let release = File.ReadLines "RELEASE_NOTES.md" |> ReleaseNotesHelper.parseReleaseNotes

// Targets
Target "?" (fun _ ->
    printfn " *********************************************************"
    printfn " *        Available options (call 'build <Target>')      *"
    printfn " *********************************************************"
    printfn " [Build]"
    printfn "  > BuildTests"
    printfn "  > BuildApp"
    printfn " "
    printfn " [Run]"
    printfn "  > RunTests"
    printfn " "
    printfn " [Help]"
    printfn "  > ?"
    printfn " "
    printfn " *********************************************************"
)

Target "AssemblyInfo" <| fun () ->
    for file in !! ("./src/**/AssemblyInfo*.fs") do
        let version = release.AssemblyVersion
        let dirName = FileInfo(file).Directory.Name
        CreateFSharpAssemblyInfo file
           [ Attribute.Title dirName
             Attribute.Product title
             Attribute.Description description
             Attribute.Version version
             Attribute.FileVersion version]

Target "CleanApp" (fun _ ->
    CleanDir appBuildDir
)

Target "CleanTests" (fun _ ->
    CleanDir testsBuildDir
)

Target "BuildApp" (fun _ ->
    for file in !! (appSrcDir + "**/*.fsproj") do
        let dir = appBuildDir + FileInfo(file).Directory.Name
        MSBuildRelease dir "Build" [file] |> Log "Build-Output:"
)

Target "BuildTests" (fun _ ->
    for file in !! (testsSrcDir + "/**/*.fsproj") do
        let dir = testsBuildDir + FileInfo(file).Directory.Name
        MSBuildRelease dir "Build" [file] |> Log "TestBuild-Output:"
)

Target "RunTests" (fun _ ->
    for file in !! (testsBuildDir + "**/*.Tests.dll") do
        let dir = FileInfo(file).DirectoryName
        NUnit3 (fun p -> { p with ResultSpecs = [dir + "\TestResult.xml"]}) [file]
)


// Dependencies
"CleanTests" ==> "BuildTests"
"BuildTests"  ==> "RunTests"
"CleanApp" ==> "AssemblyInfo" ==> "BuildApp"

// start build
RunTargetOrDefault "?"