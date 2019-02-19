module DomainTests

open System

open Expecto
open FsChess.Result
open FsChess.Domain
open Expecto.Flip
open System

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

            gameState.Board |> Expect.equal "Something went wrong drawing the board in initialiseGame()" ``Initial Board State``
            gameState.NextMove |> Expect.equal "Something went wrong setting correct player in initialiseGame()" WhiteToMove
            gameState.Status |> Expect.equal "Something went wrong setting correct status in initialiseGame()" InProgress


        testCase "Distance is calculated correctly" <| fun _ ->
            let move : ProposedMove = {
                From = (A,Two)
                To = (A,Three)
            }

            let expected = {Horizontal = 0; Vertical = 1;}

            (getDistance move) |> Expect.equal "Pawn should only have moved one sqaure forward" expected

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

            (board.[originalSqaure]) |> Expect.equal "Pawn not in correct place" (Some piece)

            let board' = updateBoard board move
            let movedPiece = {piece with Rank = Pawn Moved}

            (board'.[originalSqaure]) |> Expect.equal "Square should now be empty" None
            (board'.[targetSquare]) |> Expect.equal "Square should now have moved pawn in it" (Some movedPiece)

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

            (board.[originalSqaure]) |> Expect.equal "Knight not in correct place" (Some piece)

            let board' = updateBoard board move

            (board'.[originalSqaure]) |> Expect.equal "Square should now be empty" None
            (board'.[targetSquare]) |> Expect.equal "Square should now have moved Knight in it" (Some piece)

        testCase "validatePieceSelected throws error on empty square" <| fun _ ->
            let game = initialiseGame()
            let board = game.Board

            let response = validatePieceSelected board { From = (C, Six); To = (D, Six) }

            response |> Expect.equal "There should not be a piece here" (Error "You must select a piece")

        testCase "validatePieceSelected returns OK when square populated" <| fun _ ->
            let game = initialiseGame()
            let board = game.Board

            let input = { From = (A, Two); To = (C, Three) }
            let piece = board.[input.From]

            let expected = { SelectedPiece = piece.Value; From = input.From; To = input.To }

            let response = validatePieceSelected board input

            response |> Expect.equal "There should not be a piece here" (Ok expected)         

        testCase "validateNoFriendlyFire won't allow white rook to capture own pawn" <| fun _ ->
            let game = initialiseGame()
            let board = game.Board

            let move = { SelectedPiece = { Player = White; Rank = Rook }; From = (A, One); To = (A, Two) }

            let expected = Error "You cannot capture your own piece"

            let actual = validateNoFriendlyFire board game.NextMove move

            actual |> Expect.equal "Should not be allowed capature own piece" expected

        testCase "validateNoFriendlyFire won't allow black rook to capture own pawn" <| fun _ ->
            let game = { initialiseGame() with NextMove = BlackToMove }
            let board = game.Board

            let move = { SelectedPiece = { Player = Black; Rank = Rook }; From = (A, Eight); To = (A, Seven) }

            let expected = Error "You cannot capture your own piece"

            let actual = validateNoFriendlyFire board game.NextMove move

            actual |> Expect.equal "Should not be allowed capature own piece" expected

        testCase "validateNoFriendlyFire will allow white rook to capture black pawn" <| fun _ ->
            let game = initialiseGame()

            let from = (D, Three)
            let target = (D, Four)

            let rook = { Player = White; Rank = Rook }
            let pawn = { Player = Black; Rank = Pawn Moved }

            let board =
                game.Board
                    .Add(from, Some rook)
                    .Add(target, Some pawn)

            let move = { SelectedPiece = rook; From = from; To = target }

            let expected = Ok move

            let actual = validateNoFriendlyFire board game.NextMove move

            actual |> Expect.equal "Should be allowed capature enemy piece" expected

        testCase "validateNoFriendlyFire will allow white rook to move to empty square" <| fun _ ->
            let game = initialiseGame()

            let from = (D, Three)
            let target = (D, Four)

            let rook = { Player = White; Rank = Rook }

            let board =
                game.Board
                    .Add(from, Some rook)
                    .Add(target, None)

            let move = { SelectedPiece = rook; From = from; To = target }

            let expected = Ok move

            let actual = validateNoFriendlyFire board game.NextMove move

            actual |> Expect.equal "Should be allowed move to empty square" expected              

    ]
