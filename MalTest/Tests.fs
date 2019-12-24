module Tests

open System
open Xunit
open Mal
open Mal.Parse

[<Fact>]
let ``Integer Example Definition`` () =
    let c = @"
declare int by 1:int, `+1`:int->int, `-1`:int->int
such that | `+1` (`-1` x) -> x | `-1` (`+1` x) -> x";
    match parseDeclaration c with
    | Ok a -> 
        let expected =
            let i = Category "int"
            let fii = Lambda (i, i)
            let ``1`` = "1", i, None
            let ``+1`` = "+1", fii, Some (TermPattern ("+1",TermPattern ("-1",BindPattern "x")), Var "x")
            let ``-1`` = "-1", fii, Some (TermPattern ("-1",TermPattern ("+1",BindPattern "x")), Var "x")
            CategoryDeclaration ("int",[``1``; ``+1``; ``-1``])
        Assert.Equal (expected, a)
    | Error e -> 
        failwithf "%O" e

[<Fact>]
let ``Integer Example Definition 2`` () =
    let c = @"
declare int by 1:int, `+1`:int->int, `-1`:int->int
such that | `+1` (`-1` x) -> x | `-1` (`+1` x) -> x

`+1` (`-1` (`-1` 1))";
    match parseBlock c with
    | Ok a -> 
        Assert.Equal (Term ("-1", Atom "1"), Block.execute Map.empty a)    
    | Error e -> 
        failwithf "%O" e

[<Fact>]
let ``Match Expression`` () =
    let c = @"
declare int by 1:int, `+1`:int->int, `-1`:int->int
such that | `+1` (`-1` x) -> x | `-1` (`+1` x) -> x

declare sign by zero:sign, positive:sign, negetive:sign

declare x = `+1` (`+1` (`-1` 1))
(match
| =0 -> zero
| `+1` x -> positive
| `-1` x -> negetive) x"
    match parseBlock c with
    | Ok block -> 
        match Block.typeOf Map.empty block with
        | Ok actual ->
            let expected = Category "sign"
            Assert.Equal (expected, actual)
            do
                let actual = Block.execute Map.empty block
                let expected = Atom "positive"
                Assert.Equal (expected, actual)
        | Error e -> failwithf "%O" e
    | Error e -> 
        failwithf "%O" e