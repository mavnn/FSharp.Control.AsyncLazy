module FSharp.Control.AsyncLazy

open System

type private Message<'a> =
  | Force of AsyncReplyChannel<Choice<'a, exn>>
  | Start

let inline private agentBody creator (inbox : MailboxProcessor<Message<_>>) =
    let value = ref None

    let evaluate () =
      async {
        try
          let! v = creator
          value := Some (Choice1Of2 v)
        with
        | e ->
          value := Some (Choice2Of2 e)
      }

    let rec loop started =
        async {
            let! msg = inbox.Receive()
            match msg with
            | Force chan ->
              match !value with
              | Some v ->
                  chan.Reply v
                  return! loop started
              | None ->
                  match started with
                  | Some work ->
                      do! work
                      chan.Reply ((!value).Value)
                      return! loop started
                  | None ->
                      do! evaluate ()
                      chan.Reply ((!value).Value)
                      return! loop started
            | Start ->
              match !value with
              | Some _ -> return! loop started
              | None ->
                  match started with
                  | Some _ ->
                      return! loop started
                  | None ->
                      let! started' = Async.StartChild(evaluate())
                      return! loop <| Some started'
        }
    loop None

type AsyncLazy<'a> (creator : Async<'a>) =
    let agent =
        MailboxProcessor.Start (agentBody creator)
    new () = AsyncLazy(async { return Activator.CreateInstance<'a>() })
    member __.Force () =
        async {
            let! r = agent.PostAndAsyncReply Force
            return
                match r with
                | Choice1Of2 v -> v
                | Choice2Of2 e -> raise e
        }
    member __.Start () =
        agent.Post Start

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module AsyncLazy =
    let Create creator = AsyncLazy(creator)
    let CreateFromValue value = AsyncLazy(async { return value })
    let Parallel (lazies : #seq<AsyncLazy<_>>) =
        lazies
        |> Seq.map (fun l -> l.Force())
        |> Async.Parallel
        |> Create
    let map f (l : AsyncLazy<_>) =
        async {
            let! r = l.Force()
            return f r
        } |> Create
