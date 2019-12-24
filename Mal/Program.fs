// Learn more about F# at http://fsharp.org

open System
open Mal
open Mal.ParseInternal
open APC.Main

[<EntryPoint>]
let main argv =
    use file = new System.IO.StreamReader ("C:/Users/משתמש/source/repos/Mal/Mal/TextFile1.txt")
    let i = input (file.ReadToEnd ())
    match run pblock i with
    | Ok (a, i) -> 
        printfn "Output: %O" a
        printfn "%O" (Block.execute Map.empty a)
    | Fail (i,l,m) -> 
        printfn "Parser %s failed at %O with %O" l i m
    0 // return an integer exit code
