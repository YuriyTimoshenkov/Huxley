namespace Huxley.Storage

type IStorage =
    abstract member Add: string -> string -> unit
    abstract member Get: string -> string
    abstract member Name: string
    abstract member Remove: string -> unit
    abstract member CreateEmpty: string -> IStorage
    abstract member GetContent: (string*string) list


type MemoryStorage(name) =
    
    let mutable storage = Map.empty<string,string>;

    interface IStorage with
        member this.Add key value =
            let storage = storage <- storage.Add (key, value)
            printf "Added item with key = '%s' and value = '%s'" key value 

        member this.Get key =
            storage.[key]

        member this.Name = name

        member this.Remove key = 
            storage <- storage.Remove key

        member this.GetContent = storage |> Map.toList

        member this.CreateEmpty name = new MemoryStorage(name) :> IStorage



