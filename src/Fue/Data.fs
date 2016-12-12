module Fue.Data

let init = Map.empty
let add (key:string) (value:obj) (map:Map<string, obj>) = map.Add(key, value)
let addBatch (data:(string * obj) list) (map:Map<string, obj>) = data |> List.fold (fun acc item -> acc |> add (fst item) (snd item)) map
let get (key:string) (map:Map<string, obj>) = map.Item key
let tryGet (key:string) (map:Map<string, obj>) = map.TryFind key
let remove (key:string) (map:Map<string, obj>) = map.Remove key