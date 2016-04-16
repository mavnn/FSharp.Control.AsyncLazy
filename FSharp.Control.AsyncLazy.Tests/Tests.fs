module FSharp.Control.AsyncLazy.Tests

open FSharp.Control.AsyncLazy
open NUnit.Framework

let run = Async.RunSynchronously

[<Test>]
let ``Can create an async lazy and force it`` () =
    let it = AsyncLazy(async { return 10 })
    Assert.AreEqual(10, it.Force() |> run)

[<Test>]
[<ExpectedException>]
let ``Exceptions during force should be propagated`` () =
    let it = AsyncLazy(async { failwith "boo"; return 10 })
    it.Force() |> run |> ignore

[<Test>]
let ``Calling force multiple times only runs creator once`` () =
    let count = ref 0
    let it = AsyncLazy(async { count := !count + 1; return 10 })
    for i in [0..10] do
        it.Force() |> run |> ignore
    Assert.AreEqual(1, !count)

[<Test>]
let ``Calling force multiple times concurrently only runs creator once`` () =
    let count = ref 0
    let it = AsyncLazy(async { count := !count + 1; return 10 })
    [ for _ in [0..10] -> it.Force() ]
    |> Async.Parallel
    |> run
    |> ignore
    Assert.AreEqual(1, !count)

[<Test>]
let ``Parallel method only evaluates each async lazy once`` () =
    let count = ref 0
    let it = AsyncLazy(async { count := !count + 1; return 10 })
    let l =
        [ for _ in [0..10] -> it]
        |> AsyncLazy.Parallel
    run (l.Force() |> Async.Ignore)
    Assert.AreEqual(1, !count)

[<Test>]
[<ExpectedException>]
let ``Parallel method should throw if any force fails`` () =
    let count = ref 0
    let it = AsyncLazy(async { count := !count + 1; return 10 })
    let l =
        [ for _ in [0..10] -> it]
    let l' =
        (AsyncLazy(async {failwith "boo"; return 10}))::l
        |> AsyncLazy.Parallel
    run (l'.Force() |> Async.Ignore)

[<Test>]
let ``Calling start multiple times concurrently only runs creator once`` () =
    let count = ref 0
    let it = AsyncLazy(async { count := !count + 1; return 10 })
    [ for _ in [0..10] -> async { it.Start() } ]
    |> Async.Parallel
    |> run
    |> ignore
    Async.RunSynchronously (it.Force() |> Async.Ignore)
    Assert.AreEqual(1, !count)