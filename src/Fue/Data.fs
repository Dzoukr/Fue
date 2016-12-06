module Fue.Data

let init = Map.empty
let add (key:string) (value:obj) (map:Map<string, obj>) = map.Add(key, value)
let get (key:string) (map:Map<string, obj>) = map.Item key
let tryGet (key:string) (map:Map<string, obj>) = map.TryFind key
let remove (key:string) (map:Map<string, obj>) = map.Remove key