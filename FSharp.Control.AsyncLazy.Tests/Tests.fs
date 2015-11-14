module FSharp.Control.AsyncLazy.Tests

open FSharp.Control.AsyncLazy
open NUnit.Framework

let run = Async.RunSynchronously

[<Test>]
let ``Can create an async lazy and force it`` () =
    let it = AsyncLazy(fun () -> async { return 10 })
    Assert.AreEqual(10, it.Force() |> run)

[<Test>]
let ``Exceptions during force should be propagated`` () =
    let it = AsyncLazy(fun () -> async { failwith "boo"; return 10 })
    Assert.Throws<System.Exception>(fun () -> it.Force() |> run |> ignore)
    |> ignore

[<Test>]
let ``Calling force multiple times only runs creator once`` () =
    let count = ref 0
    let it = AsyncLazy(fun () -> async { count := !count + 1; return 10 })
    for i in [0..10] do
        it.Force() |> run |> ignore
    Assert.AreEqual(1, !count)

[<Test>]
let ``Calling force multiple times concurrently only runs creator once`` () =
    let count = ref 0
    let it = AsyncLazy(fun () -> async { count := !count + 1; return 10 })
    [ for _ in [0..10] -> it.Force() ]
    |> Async.Parallel
    |> run
    |> ignore
    Assert.AreEqual(1, !count)