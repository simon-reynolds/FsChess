namespace FsChess.Tests

module DomainTests =

    open Expecto
    open FsChess.Domain
    open Expecto.Flip

    let ``Initial Board State`` =
        [ ((A, One), Some { Player = White; Rank = Rook })
          ((B, One), Some { Player = White; Rank = Knight })
          ((C, One), Some { Player = White; Rank = Bishop })
          ((D, One), Some { Player = White; Rank = Queen })
          ((E, One), Some { Player = White; Rank = King })
          ((F, One), Some { Player = White; Rank = Bishop })
          ((G, One), Some { Player = White; Rank = Knight })
          ((H, One), Some { Player = White; Rank = Rook })

          ((A, Two), Some { Player = White; Rank = Pawn NotMoved })
          ((B, Two), Some { Player = White; Rank = Pawn NotMoved })
          ((C, Two), Some { Player = White; Rank = Pawn NotMoved })
          ((D, Two), Some { Player = White; Rank = Pawn NotMoved })
          ((E, Two), Some { Player = White; Rank = Pawn NotMoved })
          ((F, Two), Some { Player = White; Rank = Pawn NotMoved })
          ((G, Two), Some { Player = White; Rank = Pawn NotMoved })
          ((H, Two), Some { Player = White; Rank = Pawn NotMoved })

          ((A, Three), None)
          ((B, Three), None)
          ((C, Three), None)
          ((D, Three), None)
          ((E, Three), None)
          ((F, Three), None)
          ((G, Three), None)
          ((H, Three), None)

          ((A, Four), None)
          ((B, Four), None)
          ((C, Four), None)
          ((D, Four), None)
          ((E, Four), None)
          ((F, Four), None)
          ((G, Four), None)
          ((H, Four), None)

          ((A, Five), None)
          ((B, Five), None)
          ((C, Five), None)
          ((D, Five), None)
          ((E, Five), None)
          ((F, Five), None)
          ((G, Five), None)
          ((H, Five), None)

          ((A, Six), None)
          ((B, Six), None)
          ((C, Six), None)
          ((D, Six), None)
          ((E, Six), None)
          ((F, Six), None)
          ((G, Six), None)
          ((H, Six), None)

          ((A, Seven), Some { Player = Black; Rank = Pawn NotMoved })
          ((B, Seven), Some { Player = Black; Rank = Pawn NotMoved })
          ((C, Seven), Some { Player = Black; Rank = Pawn NotMoved })
          ((D, Seven), Some { Player = Black; Rank = Pawn NotMoved })
          ((E, Seven), Some { Player = Black; Rank = Pawn NotMoved })
          ((F, Seven), Some { Player = Black; Rank = Pawn NotMoved })
          ((G, Seven), Some { Player = Black; Rank = Pawn NotMoved })
          ((H, Seven), Some { Player = Black; Rank = Pawn NotMoved })

          ((A, Eight), Some { Player = Black; Rank = Rook })
          ((B, Eight), Some { Player = Black; Rank = Knight })
          ((C, Eight), Some { Player = Black; Rank = Bishop })
          ((D, Eight), Some { Player = Black; Rank = Queen })
          ((E, Eight), Some { Player = Black; Rank = King })
          ((F, Eight), Some { Player = Black; Rank = Bishop })
          ((G, Eight), Some { Player = Black; Rank = Knight })
          ((H, Eight), Some { Player = Black; Rank = Rook })

          ]
        |> Map

    [<Tests>]
    let tests =
        testList
            "Initialisation"
            [ testCase "Board is initialised correctly"
              <| fun _ ->
                  let gameState = initialiseGame ()

                  gameState.Board
                  |> Expect.equal "Something went wrong drawing the board in initialiseGame()" ``Initial Board State``

                  gameState.CurrentPlayer
                  |> Expect.equal "Something went wrong setting correct player in initialiseGame()" White

                  gameState.Status
                  |> Expect.equal "Something went wrong setting correct status in initialiseGame()" InProgress ]
