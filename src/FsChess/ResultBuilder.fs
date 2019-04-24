namespace FsChess

module Result =

    let (>>=) result binder = Result.bind binder result

    /// Return result1 if Ok, result2 otherwise
    let (<->) result1 result2 =
        match result1 with
        | Ok _ -> result1
        | Error _ -> result2

    let ofOption error = function Some s -> Ok s | None -> Error error

    type ResultBuilder() =
        member __.Return(x) = Ok x

        member __.ReturnFrom(m: Result<_, _>) = m

        member __.Bind(m, f) = Result.bind f m
        member __.Bind((m, error): (Option<'T> * 'E), f) = m |> ofOption error |> Result.bind f

        member __.Zero() = None

        member __.Combine(m, f) = Result.bind f m

        member __.Delay(f: unit -> _) = f

        member __.Run(f) = f()

        member __.TryWith(m, h) =
            try __.Run(m) with ex -> h ex

        member __.TryFinally(m : unit -> Result<_, _>, compensation) =
            try __.Run(m)
            finally compensation()

    let result = ResultBuilder()
