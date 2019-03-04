module ResultBuilderTests

open System

open Expecto
open FsChess.Result

type MyErr = | Err1

let result1 str : Result<string, MyErr> = Ok str
let result2 str : Result<string, MyErr> = match str with | "pass" -> Ok "This test passes" | _ -> Error Err1

let resultGoesBoom () : Result<string, string> =
    failwith "Boom!"

let resultToCompute () : Result<string, string> =
    Ok "pass"

[<Tests>]
let resultBuilderTests =
    testList "ResultBuilder" [
        testCase "Works as expected - success" <| fun _ ->

            let aa : Result<string, MyErr> =
                result {
                    let! a = result1 "pass"
                    let! b = result2 a

                    return b
                }

            Expect.equal aa (Ok "This test passes") "Should match"

        testCase "Works as expected - fail" <| fun _ ->
            let aa =
                result {
                    let! a = result1 "fail"
                    let! b = result2 a

                    return b
                }

            Expect.equal aa (Error Err1) "Should match"

        testCase "Works as expected - binding" <| fun _ ->
            let aa =
                result {
                    let! a = "pass" |> result1 >>= result2
                    return a
                }

            Expect.equal aa (Ok "This test passes") "Should match"

        testCase "Works as expected - Zero" <| fun _ ->
            Expect.equal (result.Zero()) None "Should match"

        testCase "Works as expected - Combine" <| fun _ ->
            let a = "pass" |> result1 >>= result2
            let b = result.Combine("pass" |> result1, result2)
            Expect.equal a b "Should match"

        testCase "Works as expected - Bind" <| fun _ ->
            let err = Err1
            let b = result.Bind((Some "fail", err), result2)
            Expect.equal b (Error err) "Should match"

        testCase "Works as expected with exception - TryWith" <| fun _ ->
            let b = result.TryWith(resultGoesBoom, fun e -> Error "That went boom")
            Expect.equal b (Error "That went boom") "Should match"

        testCase "Works as expected with exception - TryFinally" <| fun _ ->

            let mutable assertion = false

            let b = result.TryFinally(resultToCompute, fun () -> assertion <- true)
            Expect.equal assertion true "Should match"

    ]
