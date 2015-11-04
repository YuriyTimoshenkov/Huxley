namespace Huxley.StorageManager

open Huxley.Storage

type IStorageManager =
    abstract member Add: string -> unit
    abstract member Remove: string -> unit
    abstract member GetAllStorages: Map<string, IStorage>

type SimpleStorageManager(storageFactory: IStorage) =
    let mutable storageCollection = Map.empty<string,IStorage>
    let storageFactory = storageFactory

    interface IStorageManager with
        member this.Add name = storageCollection <- storageCollection.Add (name, storageFactory.CreateEmpty name)
        member this.Remove name = storageCollection.Remove name |> ignore 
        member this.GetAllStorages = storageCollection
            