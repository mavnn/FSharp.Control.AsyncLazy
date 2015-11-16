module FSharp.Control.AsyncLazy

open System

let inline private agentBody creator (inbox : MailboxProcessor<AsyncReplyChannel<_>>) =
    let rec loop value =
        async {
            let! chan = inbox.Receive()
            match value with
            | Some v ->
                chan.Reply (Choice1Of2 v)
                return! loop value
            | None ->
                try
                    let! v = creator
                    chan.Reply (Choice1Of2 v)
                    return! loop (Some v)
                with
                | e -> 
                    chan.Reply (Choice2Of2 e)
        }
    loop None

type AsyncLazy<'a> (creator : Async<'a>) =
    let agent = 
        MailboxProcessor.Start (agentBody creator)
    new () = AsyncLazy(async { return Activator.CreateInstance<'a>() })
    member __.Force () =
        async {
            let! r = agent.PostAndAsyncReply id
            return
                match r with
                | Choice1Of2 v -> v
                | Choice2Of2 e -> raise e
        }

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module AsyncLazy =
    let Create creator = AsyncLazy(creator)
    let CreateFromValue value = AsyncLazy(async { return value })
