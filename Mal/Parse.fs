module Mal.Parse

open Mal
open Mal.ParseInternal
open APC.Main

type ParseError = 
    {Message:string;Col:int;Row:int}
    override t.ToString () = sprintf "%s\nAt character:%d in line:%d " t.Message t.Col t.Row

let private wrapParser p = 
    input>>run p>>function 
    | Ok (a,i) -> 
        match tryGet i with
        | None -> Result.Ok a
        | Some _ -> Error {Message="Expected EOF";Col=i.Pos.Col;Row=i.Pos.Row}
    | Fail (i,l,m) ->
        Error {Message=sprintf "Failed parsing %s: %s." l m;Col=i.Pos.Col;Row=i.Pos.Row}

let private allowWhiteSpace p = pspacesOrLines >>. p .>> pspacesOrLines

let parseExpr input = wrapParser (allowWhiteSpace pexpr) input
let parseDeclaration input = wrapParser (allowWhiteSpace pdeclare) input
let parseBlock input = wrapParser (allowWhiteSpace pblock) input