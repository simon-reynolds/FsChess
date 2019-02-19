module ResultBuilderTests

open System

open Expecto
open FsChess.Result

type MyErr = | Err1

let result1 str : Result<string, MyErr> = Ok str
let result2 str : Result<string, MyErr> = match str with | "pass" -> Ok "This test passes" | _ -> Error Err1

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

    ]

