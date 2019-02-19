module DomainTests

open System

open Expecto
open FsChess.Result
open FsChess.Domain

let ``Initial Board State`` =
    [
        ((A, One), Some {Player = White; Rank = Rook})
        ((B, One), Some {Player = White; Rank = Knight})
        ((C, One), Some {Player = White; Rank = Bishop})
        ((D, One), Some {Player = White; Rank = Queen})
        ((E, One), Some {Player = White; Rank = King})
        ((F, One), Some {Player = White; Rank = Bishop})
        ((G, One), Some {Player = White; Rank = Knight})
        ((H, One), Some {Player = White; Rank = Rook})

        ((A, Two), Some {Player = White; Rank = Pawn NotMoved})
        ((B, Two), Some {Player = White; Rank = Pawn NotMoved})
        ((C, Two), Some {Player = White; Rank = Pawn NotMoved})
        ((D, Two), Some {Player = White; Rank = Pawn NotMoved})
        ((E, Two), Some {Player = White; Rank = Pawn NotMoved})
        ((F, Two), Some {Player = White; Rank = Pawn NotMoved})
        ((G, Two), Some {Player = White; Rank = Pawn NotMoved})
        ((H, Two), Some {Player = White; Rank = Pawn NotMoved})

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

        ((A, Seven), Some {Player = Black; Rank = Pawn NotMoved})
        ((B, Seven), Some {Player = Black; Rank = Pawn NotMoved})
        ((C, Seven), Some {Player = Black; Rank = Pawn NotMoved})
        ((D, Seven), Some {Player = Black; Rank = Pawn NotMoved})
        ((E, Seven), Some {Player = Black; Rank = Pawn NotMoved})
        ((F, Seven), Some {Player = Black; Rank = Pawn NotMoved})
        ((G, Seven), Some {Player = Black; Rank = Pawn NotMoved})
        ((H, Seven), Some {Player = Black; Rank = Pawn NotMoved})

        ((A, Eight), Some {Player = Black; Rank = Rook})
        ((B, Eight), Some {Player = Black; Rank = Knight})
        ((C, Eight), Some {Player = Black; Rank = Bishop})
        ((D, Eight), Some {Player = Black; Rank = Queen})
        ((E, Eight), Some {Player = Black; Rank = King})
        ((F, Eight), Some {Player = Black; Rank = Bishop})
        ((G, Eight), Some {Player = Black; Rank = Knight})
        ((H, Eight), Some {Player = Black; Rank = Rook})

    ] |> Map

[<Tests>]
let tests =
    testList "Initialisation" [
        testCase "Board is initialised correctly" <| fun _ ->
            let gameState = initialiseGame()

            Expect.equal gameState.Board ``Initial Board State`` "Something went wrong drawing the board in initialiseGame()"
            Expect.equal gameState.NextMove WhiteToMove "Something went wrong setting correct player in initialiseGame()"
            Expect.equal gameState.Status InProgress "Something went wrong setting correct status in initialiseGame()"


        testCase "Distance is calculated correctly" <| fun _ ->
            let move : ProposedMove = {
                From = (A,Two)
                To = (A,Three)
            }

            let expected = {Horizontal = 0; Vertical = 1;}

            Expect.equal (getDistance move) expected "Pawn should only have moved one sqaure forward"

        testCase "Board updated correctly when pawn moved" <| fun _ ->
            let originalSqaure = (A, Two)
            let targetSquare = (A, Three)
            let piece = {Player = White; Rank = Pawn NotMoved}

            let game = initialiseGame()
            let board = game.Board

            let move = {
                From = originalSqaure
                To = targetSquare
                Piece = piece
            }

            Expect.equal (board.[originalSqaure]) (Some piece) "Pawn not in correct place"

            let board' = updateBoard board move
            let movedPiece = {piece with Rank = Pawn Moved}

            Expect.equal (board'.[originalSqaure]) None "Square should now be empty"
            Expect.equal (board'.[targetSquare]) (Some movedPiece) "Square should now have moved pawn in it"

        testCase "Board updated correctly when knight moved" <| fun _ ->
            let originalSqaure = (B, One)
            let targetSquare = (A, Three)
            let piece = {Player = White; Rank = Knight}

            let game = initialiseGame()
            let board = game.Board

            let move = {
                From = originalSqaure
                To = targetSquare
                Piece = piece
            }

            Expect.equal (board.[originalSqaure]) (Some piece) "Knight not in correct place"

            let board' = updateBoard board move

            Expect.equal (board'.[originalSqaure]) None "Square should now be empty"
            Expect.equal (board'.[targetSquare]) (Some piece) "Square should now have moved Knight in it"

    ]
