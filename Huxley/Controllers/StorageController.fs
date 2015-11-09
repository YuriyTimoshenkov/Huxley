namespace Huxley.Controllers

open System.Web.Http
open Huxley.StorageManager
open Huxley

[<RoutePrefix("api/db")>]
type StorageController(storageManager: IStorageManager) =
    inherit ApiController() 

    let storageManager = storageManager

    new() = StorageController(Global.StorageManager)

    [<Route("AddStorage/{name}")>]
    [<HttpGet()>]
    member this.AddStorage name = 
        storageManager.Add name
        this.Ok name

    [<Route("RemoveStorage/{name}")>]
    [<HttpGet()>]
    member this.RemoveStorage name =
        storageManager.Remove name
        this.Ok name

    [<Route("GetAllStorageNames")>]
    [<HttpGet()>]
    member this.GetAllStorageNames () =
        storageManager.GetAllStorages |> Map.toList  |> List.map( fun (k,v) -> k ) |> this.Ok

    [<Route("{dbName}/Add/{key}/{value}")>]
    [<HttpGet()>]
    member this.AddValue dbName key value = 
        storageManager.GetAllStorages.[dbName].Add key value
        this.Ok key

    [<Route("{dbName}/Remove/{key}")>]
    [<HttpGet()>]
    member this.RemoveValue dbName key =
        storageManager.GetAllStorages.[dbName].Remove key
        this.Ok key

    [<Route("{dbName}/Get/{key}")>]
    [<HttpGet()>]
    member this.GetValue dbName key =
        key |> storageManager.GetAllStorages.[dbName].Get |> this.Ok

    [<Route("{dbName}/GetContent")>]
    [<HttpGet()>]
    member this.GetDbContent dbName =
        storageManager.GetAllStorages.[dbName].GetContent |> this.Ok

