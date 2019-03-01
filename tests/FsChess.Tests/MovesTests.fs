module MoveTests

open Expecto
open FsChess.Domain
open FsChess.Moves
open Expecto.Flip

[<Tests>]
let moveTests =
    testList "Moves" [
        testCase "Distance is calculated correctly" <| fun _ ->
            let move : ProposedMoveWithKnownPiece = {
                SelectedPiece = { Player = White; Rank = Pawn Moved }
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
                CapturedPiece = None
            }

            (board.[originalSqaure]) |> Expect.equal "Pawn not in correct place" (Some piece)

            let state' = updateGameState game move
            let board' = state'.Board
            let movedPiece = {piece with Rank = Pawn Moved}

            (board'.[originalSqaure]) |> Expect.equal "Square should now be empty" None
            (board'.[targetSquare]) |> Expect.equal "Square should now have moved pawn in it" (Some movedPiece)

            state'.MoveHistory.Length |> Expect.equal "Should only have one move in history" 1
            state'.MoveHistory.Head |> Expect.equal "History should contain the move that was just made" move

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
                CapturedPiece = None
            }

            (board.[originalSqaure]) |> Expect.equal "Knight not in correct place" (Some piece)

            let state' = updateGameState game move
            let board' = state'.Board

            (board'.[originalSqaure]) |> Expect.equal "Square should now be empty" None
            (board'.[targetSquare]) |> Expect.equal "Square should now have moved Knight in it" (Some piece)
    ]

let moveWhitePawn (board : Board) =
    let selectedPosition = (A, Two)
    let piece = board.[selectedPosition]
    { SelectedPiece = piece.Value; From = selectedPosition; To = (A, Three) }

let moveBlackPawn (board : Board) =
    let selectedPosition = (A, Seven)
    let piece = board.[selectedPosition]
    { SelectedPiece = piece.Value; From = selectedPosition; To = (A, Six) }

[<Tests>]
let validationTests =
    testList "Validation" [
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

        testCase "validatePieceIsGood allows White to select White" <| fun _ ->
            let game = { initialiseGame() with CurrentPlayer = White }
            let input = moveWhitePawn game.Board

            let response = validatePieceIsGood game.CurrentPlayer input
            response |> Expect.equal "White can only move white pieces" (Ok input)

        testCase "validatePieceIsGood allows Black to select Black" <| fun _ ->
            let game = { initialiseGame() with CurrentPlayer = Black }
            let input = moveBlackPawn game.Board

            let response = validatePieceIsGood game.CurrentPlayer input
            response |> Expect.equal "Black can only move black pieces" (Ok input)

        testCase "validatePieceIsGood does not allow White to select Black" <| fun _ ->
            let game = { initialiseGame() with CurrentPlayer = White }
            let input = moveBlackPawn game.Board
            let expected = Error "You cannot move another player's piece"

            let response = validatePieceIsGood game.CurrentPlayer input
            response |> Expect.equal "White can only move white pieces" expected

        testCase "validatePieceIsGood does not allow Black to select White" <| fun _ ->
            let game = { initialiseGame() with CurrentPlayer = Black }
            let input = moveWhitePawn game.Board
            let expected = Error "You cannot move another player's piece"

            let response = validatePieceIsGood game.CurrentPlayer input
            response |> Expect.equal "Black can only move black pieces" expected

        testCase "validateNoFriendlyFire won't allow white rook to capture own pawn" <| fun _ ->
            let game = initialiseGame()
            let board = game.Board

            let move = { SelectedPiece = { Player = White; Rank = Rook }; From = (A, One); To = (A, Two) }

            let expected = Error "You cannot capture your own piece"

            let actual = validateNoFriendlyFire board game.CurrentPlayer move

            actual |> Expect.equal "Should not be allowed capature own piece" expected

        testCase "validateNoFriendlyFire won't allow black rook to capture own pawn" <| fun _ ->
            let game = { initialiseGame() with CurrentPlayer = Black }
            let board = game.Board

            let move = { SelectedPiece = { Player = Black; Rank = Rook }; From = (A, Eight); To = (A, Seven) }

            let expected = Error "You cannot capture your own piece"

            let actual = validateNoFriendlyFire board game.CurrentPlayer move

            actual |> Expect.equal "Should not be allowed capature own piece" expected

        testCase "validateNoFriendlyFire will allow white rook to capture black pawn" <| fun _ ->
            let game = initialiseGame()

            let from, target = ((D, Three), (D, Four))

            let rook = { Player = White; Rank = Rook }
            let pawn = { Player = Black; Rank = Pawn Moved }

            let board =
                game.Board
                    .Add(from, Some rook)
                    .Add(target, Some pawn)

            let move = { SelectedPiece = rook; From = from; To = target }

            let expected = Ok move

            let actual = validateNoFriendlyFire board game.CurrentPlayer move

            actual |> Expect.equal "Should be allowed capature enemy piece" expected

        testCase "validateNoFriendlyFire will allow white rook to move to empty square" <| fun _ ->
            let game = initialiseGame()

            let from, target = ((D, Three), (D, Four))

            let rook = { Player = White; Rank = Rook }

            let board =
                game.Board
                    .Add(from, Some rook)
                    .Add(target, None)

            let move = { SelectedPiece = rook; From = from; To = target }

            let expected = Ok move

            let actual = validateNoFriendlyFire board game.CurrentPlayer move

            actual |> Expect.equal "Should be allowed move to empty square" expected

        testCase "validateMove allows moving pawn A2 -> A3 as opening move" <| fun _ ->
            let game = initialiseGame()
            let proposed = { From = (A, Two); To = (A, Three) }
            let expected = { Piece = { Player = White; Rank = Pawn NotMoved }; From = proposed.From; To = proposed.To; CapturedPiece = None }

            let actual = validateMove game proposed

            actual |> Expect.equal "Should be allowed move to empty square" (Ok expected)

        testCase "validateMove allows moving Knight B1 -> C3 as opening move" <| fun _ ->
            let game = initialiseGame()
            let proposed = { From = (B, One); To = (C, Three) }
            let expected = { Piece = {Player = White; Rank = Knight }; From = proposed.From; To = proposed.To; CapturedPiece = None }

            let actual = validateMove game proposed

            actual |> Expect.equal "Should be allowed move to empty square" (Ok expected)

        testCase "validateMove allows moving pawn A7 -> A5 as response" <| fun _ ->
            let game = initialiseGame()
            let whiteOpeningMove = { Piece = {Player = White; Rank = Pawn NotMoved }; From = (A, Two); To = (A, Three); CapturedPiece = None }

            let game' = updateGameState game whiteOpeningMove

            let proposed = { From = (A, Seven); To = (A, Five) }
            let blackOpeningMove = { Piece = {Player = Black; Rank = Pawn NotMoved }; From = proposed.From; To = proposed.To; CapturedPiece = None }
            let actual = validateMove game' proposed

            actual |> Expect.equal "Should be allowed move" (Ok blackOpeningMove)

        testCase "gameState updated after valid move" <| fun _ ->
            let game = initialiseGame()
            let validMove = { Piece = {Player = White; Rank = Pawn NotMoved }; From = (A, Two); To = (A, Three); CapturedPiece = None }

            let state = updateGameState game validMove

            state.CurrentPlayer |> Expect.equal "Next move is by Black" Black
            state.Board.[validMove.From] |> Expect.equal "Next move is by Black" None
            state.Board.[validMove.To] |> Expect.equal "Pawn has been moved" (Some {Player = White; Rank = Pawn Moved })

    ]
