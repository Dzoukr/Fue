module BuildTools

open Fake.Core

module Tools =
    open BuildHelpers

    let private findTool tool =
        match ProcessUtils.tryFindFileOnPath tool with
        | Some t -> t
        | _ ->
            let errorMsg =
                tool + " was not found in path. " +
                "Please install it and make sure it's available from your path. "
            failwith errorMsg

    let dotnet args = ("dotnet" |> findTool |> createProcess) args
    let femto args = ("femto" |> findTool |> createProcess) args
    let node args = ("node" |> findTool |> createProcess) args
    let yarn args = ("yarn" |> findTool |> createProcess) args