namespace FsChess.Tests

module MoveTests =

    open Expecto
    open FsChess.Domain
    open FsChess.Moves
    open Expecto.Flip

    let allSquares = Row.List |> Seq.allPairs Column.List

    let gameStateWithEmptyBoard player =

        let squares = allSquares |> Seq.map (fun sq -> (sq, None))

        { Board = squares |> Map
          CurrentPlayer = player
          Status = InProgress
          Message = ""
          MoveHistory = [] }

    let addPiece piece location gameState =
        { gameState with Board = gameState.Board.Add(location, Some piece) }

    let removePiece location gameState =
        { gameState with Board = gameState.Board.Add(location, None) }

    let addMoveToHistory piece oldPosition newPosition gameState =
        { gameState with
            MoveHistory =
                { Piece = piece
                  From = oldPosition
                  To = newPosition
                  CapturedPiece = None }
                :: gameState.MoveHistory }

    [<Tests>]
    let moveTests =
        testList
            "Moves"
            [ testCase "Distance is calculated correctly"
              <| fun _ ->
                  let move: ProposedMoveWithKnownPiece =
                      { SelectedPiece = { Player = White; Rank = Pawn Moved }
                        From = (A, Two)
                        To = (A, Three) }

                  let expected = { X = 0; Y = 1 }

                  (getVector move)
                  |> Expect.equal "Pawn should only have moved one sqaure forward" expected

              testCase "Board updated correctly when pawn moved"
              <| fun _ ->
                  let originalSqaure = (A, Two)
                  let targetSquare = (A, Three)
                  let piece = { Player = White; Rank = Pawn NotMoved }

                  let game = initialiseGame ()
                  let board = game.Board

                  let move =
                      { From = originalSqaure
                        To = targetSquare
                        Piece = piece
                        CapturedPiece = None }

                  (board.[originalSqaure])
                  |> Expect.equal "Pawn not in correct place" (Some piece)

                  let state' = updateGameState game move
                  let board' = state'.Board
                  let movedPiece = { piece with Rank = Pawn Moved }

                  (board'.[originalSqaure])
                  |> Expect.equal "Square should now be empty" None

                  (board'.[targetSquare])
                  |> Expect.equal "Square should now have moved pawn in it" (Some movedPiece)

                  state'.MoveHistory.Length
                  |> Expect.equal "Should only have one move in history" 1

                  state'.MoveHistory.Head
                  |> Expect.equal "History should contain the move that was just made" move

              testCase "Board updated correctly when knight moved"
              <| fun _ ->
                  let originalSqaure = (B, One)
                  let targetSquare = (A, Three)
                  let piece = { Player = White; Rank = Knight }

                  let game = initialiseGame ()
                  let board = game.Board

                  let move =
                      { From = originalSqaure
                        To = targetSquare
                        Piece = piece
                        CapturedPiece = None }

                  (board.[originalSqaure])
                  |> Expect.equal "Knight not in correct place" (Some piece)

                  let state' = updateGameState game move
                  let board' = state'.Board

                  (board'.[originalSqaure])
                  |> Expect.equal "Square should now be empty" None

                  (board'.[targetSquare])
                  |> Expect.equal "Square should now have moved Knight in it" (Some piece) ]

    let moveWhitePawn (board: Board) =
        let selectedPosition = (A, Two)
        let piece = board.[selectedPosition]

        { SelectedPiece = piece.Value
          From = selectedPosition
          To = (A, Three) }

    let moveBlackPawn (board: Board) =
        let selectedPosition = (A, Seven)
        let piece = board.[selectedPosition]

        { SelectedPiece = piece.Value
          From = selectedPosition
          To = (A, Six) }

    [<Tests>]
    let validationTests =
        testList
            "Validation"
            [ testCase "validatePieceSelected throws error on empty square"
              <| fun _ ->
                  let game = initialiseGame ()

                  let response = validatePieceSelected game { From = (C, Six); To = (D, Six) }

                  response
                  |> Expect.equal "There should not be a piece here" (Error "You must select a piece")

              testCase "validatePieceSelected returns OK when square populated"
              <| fun _ ->
                  let game = initialiseGame ()

                  let input = { From = (A, Two); To = (C, Three) }
                  let piece = game.Board.[input.From]

                  let expected =
                      { SelectedPiece = piece.Value
                        From = input.From
                        To = input.To }

                  let response = validatePieceSelected game input

                  response
                  |> Expect.equal "There should not be a piece here" (Ok expected)

              testCase "validatePieceIsGood allows White to select White"
              <| fun _ ->
                  let game = { initialiseGame () with CurrentPlayer = White }

                  let input = moveWhitePawn game.Board

                  let response = validatePieceIsGood game input

                  response
                  |> Expect.equal "White can only move white pieces" (Ok input)

              testCase "validatePieceIsGood allows Black to select Black"
              <| fun _ ->
                  let game = { initialiseGame () with CurrentPlayer = Black }

                  let input = moveBlackPawn game.Board

                  let response = validatePieceIsGood game input

                  response
                  |> Expect.equal "Black can only move black pieces" (Ok input)

              testCase "validatePieceIsGood does not allow White to select Black"
              <| fun _ ->
                  let game = { initialiseGame () with CurrentPlayer = White }

                  let input = moveBlackPawn game.Board

                  let expected = Error "You cannot move another player's piece"

                  let response = validatePieceIsGood game input

                  response
                  |> Expect.equal "White can only move white pieces" expected

              testCase "validatePieceIsGood does not allow Black to select White"
              <| fun _ ->
                  let game = { initialiseGame () with CurrentPlayer = Black }

                  let input = moveWhitePawn game.Board

                  let expected = Error "You cannot move another player's piece"

                  let response = validatePieceIsGood game input

                  response
                  |> Expect.equal "Black can only move black pieces" expected

              testCase "validateNoFriendlyFire won't allow white rook to capture own pawn"
              <| fun _ ->
                  let game = initialiseGame ()

                  let move =
                      { SelectedPiece = { Player = White; Rank = Rook }
                        From = (A, One)
                        To = (A, Two) }

                  let expected = Error "You cannot capture your own piece"

                  let actual = validateNoFriendlyFire game move

                  actual
                  |> Expect.equal "Should not be allowed capature own piece" expected

              testCase "validateNoFriendlyFire won't allow black rook to capture own pawn"
              <| fun _ ->
                  let game = { initialiseGame () with CurrentPlayer = Black }

                  let move =
                      { SelectedPiece = { Player = Black; Rank = Rook }
                        From = (A, Eight)
                        To = (A, Seven) }

                  let expected = Error "You cannot capture your own piece"

                  let actual = validateNoFriendlyFire game move

                  actual
                  |> Expect.equal "Should not be allowed capature own piece" expected

              testCase "validateNoFriendlyFire will allow white rook to capture black pawn"
              <| fun _ ->
                  let game = initialiseGame ()

                  let from, target = ((D, Three), (D, Four))

                  let rook = { Player = White; Rank = Rook }
                  let pawn = { Player = Black; Rank = Pawn Moved }

                  let board =
                      game
                          .Board
                          .Add(from, Some rook)
                          .Add(target, Some pawn)

                  let move =
                      { SelectedPiece = rook
                        From = from
                        To = target }

                  let expected = Ok move

                  let actual = validateNoFriendlyFire game move

                  actual
                  |> Expect.equal "Should be allowed capature enemy piece" expected

              testCase "validateNoFriendlyFire will allow white rook to move to empty square"
              <| fun _ ->
                  let game = initialiseGame ()

                  let from, target = ((D, Three), (D, Four))

                  let rook = { Player = White; Rank = Rook }

                  let board = game.Board.Add(from, Some rook).Add(target, None)

                  let move =
                      { SelectedPiece = rook
                        From = from
                        To = target }

                  let expected = Ok move

                  let actual = validateNoFriendlyFire game move

                  actual
                  |> Expect.equal "Should be allowed move to empty square" expected

              testCase "validateMove allows moving pawn A2 -> A3 as opening move"
              <| fun _ ->
                  let game = initialiseGame ()
                  let proposed = { From = (A, Two); To = (A, Three) }

                  let expected =
                      { Piece = { Player = White; Rank = Pawn NotMoved }
                        From = proposed.From
                        To = proposed.To
                        CapturedPiece = None }

                  let actual = validateMove game proposed

                  actual
                  |> Expect.equal "Should be allowed move to empty square" (Ok expected)

              testCase "validateMove allows moving Knight B1 -> C3 as opening move"
              <| fun _ ->
                  let game = initialiseGame ()
                  let proposed = { From = (B, One); To = (C, Three) }

                  let expected =
                      { Piece = { Player = White; Rank = Knight }
                        From = proposed.From
                        To = proposed.To
                        CapturedPiece = None }

                  let actual = validateMove game proposed

                  actual
                  |> Expect.equal "Should be allowed move to empty square" (Ok expected)

              testCase "validateMove allows moving pawn A7 -> A5 as response"
              <| fun _ ->
                  let game = initialiseGame ()

                  let whiteOpeningMove =
                      { Piece = { Player = White; Rank = Pawn NotMoved }
                        From = (A, Two)
                        To = (A, Three)
                        CapturedPiece = None }

                  let game' = updateGameState game whiteOpeningMove

                  let proposed = { From = (A, Seven); To = (A, Five) }

                  let blackOpeningMove =
                      { Piece = { Player = Black; Rank = Pawn NotMoved }
                        From = proposed.From
                        To = proposed.To
                        CapturedPiece = None }

                  let actual = validateMove game' proposed

                  actual
                  |> Expect.equal "Should be allowed move" (Ok blackOpeningMove)

              testCase "gameState updated after valid move"
              <| fun _ ->
                  let game = initialiseGame ()

                  let validMove =
                      { Piece = { Player = White; Rank = Pawn NotMoved }
                        From = (A, Two)
                        To = (A, Three)
                        CapturedPiece = None }

                  let state = updateGameState game validMove

                  state.CurrentPlayer
                  |> Expect.equal "Next move is by Black" Black

                  state.Board.[validMove.From]
                  |> Expect.equal "Next move is by Black" None

                  state.Board.[validMove.To]
                  |> Expect.equal "Pawn has been moved" (Some { Player = White; Rank = Pawn Moved })

              ]

    [<Tests>]
    let moveValidationTests =
        testList
            "Validation"
            [

              testCase "NotMoved Pawn - only valid moves allowed"
              <| fun _ ->
                  let pawn = { Player = White; Rank = Pawn NotMoved }
                  let start = (D, Two)

                  let game =
                      gameStateWithEmptyBoard White
                      |> addPiece pawn start

                  let validLocations = [ (D, Three); (D, Four) ]

                  allSquares
                  |> Seq.iter (fun sq ->
                      let move =
                          { SelectedPiece = pawn
                            From = start
                            To = sq }

                      let result = validateMoveForPiece game move

                      match validLocations |> List.contains sq with
                      | true -> result |> Expect.equal "Valid move" (Ok move)
                      | false ->
                          result
                          |> Expect.equal "Invalid move" (Error "This is not a valid move"))

              testCase "Moved Pawn - only valid moves allowed"
              <| fun _ ->
                  let pawn = { Player = White; Rank = Pawn Moved }
                  let start = (D, Five)

                  let targetPiece = { Player = Black; Rank = Queen }

                  let game =
                      gameStateWithEmptyBoard White
                      |> addPiece pawn start
                      |> addPiece targetPiece (E, Six)

                  let validLocations = [ (D, Six); (E, Six) ]

                  allSquares
                  |> Seq.iter (fun sq ->
                      let move =
                          { SelectedPiece = pawn
                            From = start
                            To = sq }

                      let result = validateMoveForPiece game move

                      match validLocations |> List.contains sq with
                      | true -> result |> Expect.equal "Valid move" (Ok move)
                      | false ->
                          result
                          |> Expect.equal "Invalid move" (Error "This is not a valid move"))

              testCase "Rook - only valid moves allowed"
              <| fun _ ->
                  let rook = { Player = White; Rank = Rook }
                  let start = (D, Five)

                  let game =
                      gameStateWithEmptyBoard White
                      |> addPiece rook start

                  allSquares
                  |> Seq.iter (fun sq ->
                      let move =
                          { SelectedPiece = rook
                            From = start
                            To = sq }

                      let result = validateMoveForPiece game move

                      match sq with
                      | (D, Five) ->
                          result
                          |> Expect.equal "Invalid move" (Error "This is not a valid move")
                      | (D, _)
                      | (_, Five) -> result |> Expect.equal "Valid move" (Ok move)
                      | _ ->
                          result
                          |> Expect.equal "Invalid move" (Error "This is not a valid move"))

              testCase "Knight - only valid moves allowed"
              <| fun _ ->
                  let knight = { Player = White; Rank = Knight }
                  let start = (D, Five)

                  let game =
                      gameStateWithEmptyBoard White
                      |> addPiece knight start

                  let validLocations =
                      [ (B, Four)
                        (B, Six)
                        (C, Seven)
                        (C, Three)
                        (E, Seven)
                        (E, Three)
                        (F, Four)
                        (F, Six) ]

                  allSquares
                  |> Seq.iter (fun sq ->
                      let move =
                          { SelectedPiece = knight
                            From = start
                            To = sq }

                      let result = validateMoveForPiece game move

                      match validLocations |> List.contains sq with
                      | true -> result |> Expect.equal "Valid move" (Ok move)
                      | false ->
                          result
                          |> Expect.equal "Invalid move" (Error "This is not a valid move"))

              testCase "Bishop - only valid moves allowed"
              <| fun _ ->
                  let bishop = { Player = White; Rank = Bishop }
                  let start = (D, Five)

                  let game =
                      gameStateWithEmptyBoard White
                      |> addPiece bishop start

                  let validLocations =
                      [ (A, Eight)
                        (B, Seven)
                        (C, Six)
                        (E, Four)
                        (F, Three)
                        (G, Two)
                        (H, One)
                        (G, Eight)
                        (F, Seven)
                        (E, Six)
                        (C, Four)
                        (B, Three)
                        (A, Two) ]

                  allSquares
                  |> Seq.iter (fun sq ->
                      let move =
                          { SelectedPiece = bishop
                            From = start
                            To = sq }

                      let result = validateMoveForPiece game move

                      match validLocations |> List.contains sq with
                      | true -> result |> Expect.equal "Valid move" (Ok move)
                      | false ->
                          result
                          |> Expect.equal "Invalid move" (Error "This is not a valid move"))

              testCase "Queen - only valid moves allowed"
              <| fun _ ->
                  let queen = { Player = White; Rank = Queen }
                  let start = (D, Five)

                  let game =
                      gameStateWithEmptyBoard White
                      |> addPiece queen start

                  let validDiagonalLocations =
                      [ (A, Eight)
                        (B, Seven)
                        (C, Six)
                        (E, Four)
                        (F, Three)
                        (G, Two)
                        (H, One)
                        (G, Eight)
                        (F, Seven)
                        (E, Six)
                        (C, Four)
                        (B, Three)
                        (A, Two) ]

                  allSquares
                  |> Seq.iter (fun sq ->
                      let move =
                          { SelectedPiece = queen
                            From = start
                            To = sq }

                      let result = validateMoveForPiece game move

                      match validDiagonalLocations |> List.contains sq with
                      | true -> result |> Expect.equal "Valid move" (Ok move)
                      | false ->
                          match sq with
                          | (D, Five) ->
                              result
                              |> Expect.equal "Invalid move" (Error "This is not a valid move")
                          | (D, _)
                          | (_, Five) -> result |> Expect.equal "Valid move" (Ok move)
                          | _ ->
                              result
                              |> Expect.equal "Invalid move" (Error "This is not a valid move"))



              testCase "King - only valid moves allowed"
              <| fun _ ->
                  let king = { Player = White; Rank = King }
                  let start = (D, Five)

                  let game =
                      gameStateWithEmptyBoard White
                      |> addPiece king start

                  let validLocations =
                      [ (D, Four)
                        (D, Six)
                        (C, Four)
                        (C, Five)
                        (C, Six)
                        (E, Four)
                        (E, Five)
                        (E, Six) ]

                  allSquares
                  |> Seq.iter (fun sq ->
                      let move =
                          { SelectedPiece = king
                            From = start
                            To = sq }

                      let result = validateMoveForPiece game move

                      match validLocations |> List.contains sq with
                      | true -> result |> Expect.equal "Valid move" (Ok move)
                      | false ->
                          result
                          |> Expect.equal "Invalid move" (Error "This is not a valid move")) ]

    [<Tests>]
    let moveCollisionTests =
        testList
            "Collision"
            [

              testCase "No collision means move is valid"
              <| fun _ ->
                  let bishop = { Rank = Bishop; Player = White }
                  let start = (A, One)

                  let targetPosition = (H, Eight)

                  let game =
                      gameStateWithEmptyBoard White
                      |> addPiece bishop start

                  let move =
                      { SelectedPiece = bishop
                        From = start
                        To = targetPosition }

                  let result = validateNoCollision game move

                  result
                  |> Expect.equal "No collision detected" (Ok move)

              testCase "Collision prevents move being valid"
              <| fun _ ->
                  let bishop = { Rank = Bishop; Player = White }
                  let start = (A, One)

                  let blockingPiece = { Rank = Pawn Moved; Player = Black }
                  let blockingPosition = (D, Four)

                  let targetPosition = (E, Five)

                  let game =
                      gameStateWithEmptyBoard White
                      |> addPiece bishop start
                      |> addPiece blockingPiece blockingPosition

                  let move =
                      { SelectedPiece = bishop
                        From = start
                        To = targetPosition }

                  let result = validateNoCollision game move

                  result
                  |> Expect.equal "Collision detected" (Error "There is a piece blocking this move")

              ]

    [<Tests>]
    let castlingTests =
        testList
            "Castling"
            [

              testCase "Can Castle from starting positions when nothing in the way"
              <| fun _ ->
                  let king = { Rank = King; Player = White }
                  let kingPosition = (E, One)

                  let rook = { Rank = Rook; Player = White }
                  let rookPosition = (H, One)

                  let game =
                      gameStateWithEmptyBoard White
                      |> addPiece king kingPosition
                      |> addPiece rook rookPosition

                  let move =
                      { SelectedPiece = king
                        From = kingPosition
                        To = (G, One) }

                  let result = validateMoveForPiece game move

                  result |> Expect.equal "Move is legal" (Ok move)

              testCase "Cannot Castle if king has moved"
              <| fun _ ->
                  let king = { Rank = King; Player = White }
                  let kingPosition = (E, One)

                  let rook = { Rank = Rook; Player = White }
                  let rookPosition = (H, One)

                  let game =
                      gameStateWithEmptyBoard White
                      |> addPiece king kingPosition
                      |> addPiece rook rookPosition
                      |> addMoveToHistory king kingPosition (D, One)
                      |> addMoveToHistory king (D, One) kingPosition


                  let move =
                      { SelectedPiece = king
                        From = kingPosition
                        To = (G, One) }

                  let result = validateMoveForPiece game move

                  result
                  |> Expect.equal "King has moved, cannot castle" (Error "This is not a valid move")

              testCase "Cannot Castle if rook has moved"
              <| fun _ ->
                  let king = { Rank = King; Player = Black }
                  let kingPosition = (E, Eight)

                  let rook = { Rank = Rook; Player = Black }
                  let rookPosition = (H, Eight)

                  let game =
                      gameStateWithEmptyBoard Black
                      |> addPiece king kingPosition
                      |> addPiece rook rookPosition
                      |> addMoveToHistory rook rookPosition (H, Two)
                      |> addMoveToHistory rook (H, Two) rookPosition


                  let move =
                      { SelectedPiece = king
                        From = kingPosition
                        To = (G, Eight) }

                  let result = validateMoveForPiece game move

                  result
                  |> Expect.equal "Rook has moved, cannot castle" (Error "This is not a valid move")

              testCase "Can only Castle if king is moving two places"
              <| fun _ ->
                  let king = { Rank = King; Player = Black }
                  let kingPosition = (E, Eight)

                  let rook = { Rank = Rook; Player = Black }
                  let rookPosition = (A, Eight)

                  let game =
                      gameStateWithEmptyBoard Black
                      |> addPiece king kingPosition
                      |> addPiece rook rookPosition


                  let move =
                      { SelectedPiece = king
                        From = kingPosition
                        To = (B, Eight) }

                  let result = validateMoveForPiece game move

                  result
                  |> Expect.equal "King trying to move 3 places, cannot castle" (Error "This is not a valid move") ]
