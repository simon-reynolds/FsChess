namespace FsChess

module Moves =

    module ErrorMessages =
        let MustSelectPiece = "You must select a piece"
        let MustMoveOwnPiece = "You cannot move another player's piece"
        let CannotCaptureOwnPiece =  "You cannot capture your own piece"
        let InvalidMove = "This is not a valid move"
        let MoveIsBlocked = "There is a piece blocking this move"

    open Result
    open Domain

    let getVector (move : ProposedMoveWithKnownPiece) =

        let fromX, fromY = move.From
        let toX, toY = move.To

        let deltaX = (Column.List |> List.findIndex (fun c -> c = toX)) - (Column.List |> List.findIndex (fun c -> c = fromX))
        let deltaY = (Row.List |> List.findIndex (fun c -> c = toY)) - (Row.List |> List.findIndex (fun c -> c = fromY))

        { X = deltaX; Y = deltaY }

    let markMoveAsValidated (gameState : GameState) (move : ProposedMoveWithKnownPiece) =
        Ok { Piece = move.SelectedPiece; From = move.From; To = move.To; CapturedPiece = gameState.Board.[move.To] }

    let hasPieceMovedBefore gameState piece =
        gameState.MoveHistory |> Seq.exists (fun f -> f.Piece = piece)


    let validatePieceSelected (gameState : GameState) (move : ProposedMove) =
        match gameState.Board.[move.From] with
        | Some p -> Ok { SelectedPiece = p; From = move.From; To = move.To }
        | None -> Error ErrorMessages.MustSelectPiece


    let validatePieceIsGood (gameState : GameState) (move : ProposedMoveWithKnownPiece) =
        if (gameState.CurrentPlayer = move.SelectedPiece.Player) then
            Ok move
        else Error ErrorMessages.MustMoveOwnPiece

    let validateNoFriendlyFire (gameState : GameState) (move : ProposedMoveWithKnownPiece) =

        let targetPiece = gameState.Board.[move.To]

        match targetPiece with
        | Some p ->
            if gameState.CurrentPlayer = p.Player then
                Error ErrorMessages.CannotCaptureOwnPiece
            else Ok move
        | None -> Ok move

    let validateNoCollision (gameState : GameState) (move : ProposedMoveWithKnownPiece) =
        match move.SelectedPiece.Rank with
        | Knight -> Ok move // Knights can jump over other pieces
        | _ ->

            let distance = getVector move
            let unitVector = Vector.MakeUnit distance

            let rec moveSeq startCell vector =
                seq {
                    let nextCell = startCell
                                   |> Vector.FromSquare
                                   |> (+) vector
                                   |> Vector.ToSquare

                    if nextCell.IsSome then
                        yield nextCell.Value
                        yield! moveSeq nextCell.Value vector
                }

            let valid = moveSeq move.From unitVector
                        |> Seq.takeWhile ((<>) move.To)
                        |> Seq.forall (fun move -> gameState.Board.[move].IsNone)

            if valid then
                Ok move
            else Error ErrorMessages.MoveIsBlocked

    let validateMoveForPiece (gameState : GameState) (move : ProposedMoveWithKnownPiece) =

        let isValidPawnMove (gameState : GameState) pawnState move =
            let distance = getVector move
            let direction = match gameState.CurrentPlayer with | White -> 1 | Black -> -1
            let target = gameState.Board.[move.To]

            match ((abs distance.X), (distance.Y), target, pawnState) with
            | (x, y, None, _) when x = 0 && y = (1 * direction) -> Ok move
            | (x, y, None, NotMoved) when x = 0 && y = (2 * direction) -> Ok move
            | (x, y, None, Moved) when x = 0 && y = (2 * direction) -> Error ErrorMessages.InvalidMove
            | (x, y, Some _, _) when x = 1 && y = (1 * direction) -> Ok move
            | (x, y, None, _) when x = 1 && y = (2 * direction) ->
                Error ErrorMessages.InvalidMove // en passant to be handled here
            | _ -> Error ErrorMessages.InvalidMove

        let isStraightLine move =
            let distance = getVector move
            match ((abs distance.X), (abs distance.Y)) with
            | (x, y) when x > 0 && y = 0 -> Ok move
            | (x, y) when x = 0 && y > 0 -> Ok move
            | _ -> Error ErrorMessages.InvalidMove

        let isLShape move =
            let distance = getVector move
            match ((abs distance.X), (abs distance.Y)) with
            | (2, 1) -> Ok move
            | (1, 2) -> Ok move
            | _ -> Error ErrorMessages.InvalidMove

        let isDiagonal move =
            let distance = getVector move
            match ((abs distance.X), (abs distance.Y)) with
            | (x, y) when x > 0 && y = x -> Ok move
            | _ -> Error ErrorMessages.InvalidMove

        let isStraightLineOrDiagonal move =
            (isStraightLine move) <-> (isDiagonal move)

        let isOneSquareInAnyDirection move =
            let distance = getVector move
            match ((abs distance.X), (abs distance.Y)) with
            | (1, 0) -> Ok move
            | (0, 1) -> Ok move
            | (1, 1) -> Ok move
            | _ -> Error ErrorMessages.InvalidMove

        let isValidCastlingMove gameState (move : ProposedMoveWithKnownPiece) =

            let distance = getVector move
            let kingPosition =
                match gameState.CurrentPlayer with
                | White -> (E, One)
                | Black -> (E, Eight)

            if move.SelectedPiece.Rank <> King || move.From <> kingPosition || hasPieceMovedBefore gameState move.SelectedPiece then
                Error ErrorMessages.InvalidMove
            else

                match ((abs distance.X), (abs distance.Y)) with
                | (2, 0) ->
                    let rookPosition = ((if distance.X = 2 then H else A), if move.SelectedPiece.Player = White then One else Eight) |> Square
                    let hasRookMoved = gameState.MoveHistory |> Seq.exists(fun h -> h.From = rookPosition)
                    if hasRookMoved then
                        Error ErrorMessages.InvalidMove
                    else
                        validateNoCollision gameState move
                | _ -> Error ErrorMessages.InvalidMove

        let isValidMoveForKing gameState move = (isOneSquareInAnyDirection move) <-> (isValidCastlingMove gameState move)

        match move.SelectedPiece.Rank with
        | Pawn NotMoved -> isValidPawnMove gameState NotMoved move
        | Pawn Moved -> isValidPawnMove gameState Moved move
        | Rook -> isStraightLine move
        | Knight -> isLShape move
        | Bishop -> isDiagonal move
        | Queen -> isStraightLineOrDiagonal move
        | King -> isValidMoveForKing gameState move



    let validateMove (gameState : GameState) (move : ProposedMove) =

        result {
            return!
                validatePieceSelected gameState move
                >>= validatePieceIsGood gameState
                >>= validateNoFriendlyFire gameState
                // All move validation goes here
                >>= validateMoveForPiece gameState
                >>= validateNoCollision gameState
                >>= markMoveAsValidated gameState
        }
