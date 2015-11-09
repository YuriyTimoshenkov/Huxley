namespace Huxley.Storage

open Huxley.Trees

type IStorage =
    abstract member Add: string -> string -> unit
    abstract member Get: string -> string
    abstract member Name: string
    abstract member Remove: string -> unit
    abstract member CreateEmpty: string -> IStorage
    abstract member GetContent: (string*string) list


type MemoryStorage(name) =
    
    let mutable storage = AVLTree.Empty;

    interface IStorage with
        member this.Add key value =
            storage <- AVLTree.insert key value storage

        member this.Get key =
            match AVLTree.getValue key storage with
            | Some x -> x
            | None -> "None"

        member this.Name = name

        member this.Remove key = printf "sdfs"
            //storage <- storage.Remove key

        member this.GetContent = List.Empty
        // storage |> Map.toList

        member this.CreateEmpty name = new MemoryStorage(name) :> IStorage



