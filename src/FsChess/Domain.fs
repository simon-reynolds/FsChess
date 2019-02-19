namespace FsChess

module Domain =

    open Result

    type Player = | White | Black
    type NextMove = | WhiteToMove | BlackToMove

    type Column = | A | B | C | D | E | F | G | H
        with static member List = [ A; B; C; D; E; F; G; H]
    type Row = | One | Two | Three | Four | Five | Six | Seven | Eight
        with static member List = [ One; Two; Three; Four; Five; Six; Seven; Eight]

    type Square = (Column * Row)

    type PawnState = | NotMoved | Moved

    type Rank =
        | Pawn of PawnState
        | Rook
        | Knight
        | Bishop
        | Queen
        | King

    type Piece = { Player : Player; Rank : Rank }
    type Board = Map<Square, Piece option>
    type ProposedMove = { From : Square; To : Square }
    type ProposedMoveWithKnownPiece = { SelectedPiece : Piece; From : Square; To : Square }
    type ValidatedMove = { Piece : Piece; From : Square; To : Square }
    type GameStatus = | InProgress | WhiteInCheck | WhiteInCheckmate | BlackInCheck | BlackInCheckmate | Stalemate
    type GameState = { Board : Board; NextMove : NextMove; Status : GameStatus; Message : string }
    type Distance = { Horizontal : int; Vertical : int }

    let getDistance (move : ProposedMove) =

        let fromX, fromY = move.From
        let toX, toY = move.To


        let deltaX = (Column.List |> List.findIndex (fun c -> c = toX)) - (Column.List |> List.findIndex (fun c -> c = fromX))
        let deltaY = (Row.List |> List.findIndex (fun c -> c = toY)) - (Row.List |> List.findIndex (fun c -> c = fromY))

        { Horizontal = deltaX; Vertical = deltaY}


    let initialiseGame () =

        let blackPawn = Some { Player = Black; Rank = Pawn NotMoved }
        let whitePawn = Some { Player = White; Rank = Pawn NotMoved }

        let black rank = Some { Player = Black; Rank = rank }
        let white rank = Some { Player = White; Rank = rank }

        let createRow row pieces =
            let cells =  Column.List |> List.map (fun col -> (col, row))
            List.zip cells pieces

        let board =
            Map (   (createRow Eight    [black Rook;   black Knight;   black Bishop;   black Queen;    black King;    black Bishop;   black Knight;   black Rook]) @
                    (createRow Seven    [blackPawn;    blackPawn;      blackPawn;      blackPawn;      blackPawn;     blackPawn;      blackPawn;      blackPawn]) @
                    (createRow Six      [None;         None;           None;           None;           None;          None;           None;           None]) @
                    (createRow Five     [None;         None;           None;           None;           None;          None;           None;           None]) @
                    (createRow Four     [None;         None;           None;           None;           None;          None;           None;           None]) @
                    (createRow Three    [None;         None;           None;           None;           None;          None;           None;           None]) @
                    (createRow Two      [whitePawn;    whitePawn;      whitePawn;      whitePawn;      whitePawn;     whitePawn;      whitePawn;      whitePawn]) @
                    (createRow One      [white Rook;   white Knight;   white Bishop;   white Queen;    white King;    white Bishop;   white Knight;   white Rook]) )

        {   Board = board
            NextMove = WhiteToMove
            Status = InProgress
            Message = "Welcome to FsChess!" }

    let updateBoard (board : Board) move : Board =
        let p =
            match move.Piece.Rank with
            | Pawn NotMoved -> { move.Piece with Rank = Pawn Moved }
            | _ -> move.Piece

        board.Add(move.From, None).Add(move.To, Some p)

    let markMoveAsValidated (move : ProposedMoveWithKnownPiece) =
        Ok { Piece = move.SelectedPiece; From = move.From; To = move.To}

    let validatePieceSelected (board : Board) (move : ProposedMove) =
        match board.[move.From] with
        | Some p -> Ok { SelectedPiece = p; From = move.From; To = move.To }
        | None -> Error "You must select a piece"


    let validatePieceIsGood (board : Board) nextMove (move : ProposedMoveWithKnownPiece) =
        match (nextMove, move.SelectedPiece.Player) with
        | (WhiteToMove, White)
        | (BlackToMove, Black)
            -> Ok move
        | _ -> Error "You cannot move another player's piece"

    let validateNoFriendlyFire (board : Board) nextMove (move : ProposedMoveWithKnownPiece) =

        let targetPiece = board.[move.To]

        match targetPiece with
        | Some p ->
            match (nextMove, p.Player) with
            | (WhiteToMove, Black)
            | (BlackToMove, White)
                -> Ok move
            | _ -> Error "You cannot capture your own piece"
        | None -> Ok move

    let validatePawn (board: Board) (move : ProposedMove) pawnState =

        match pawnState with
        | NotMoved -> ""
        | Moved -> ""

    let validateMove (gameState : GameState) (move : ProposedMove) =

        result {
            return!            
                validatePieceSelected gameState.Board move
                >>= validatePieceIsGood gameState.Board gameState.NextMove
                >>= validateNoFriendlyFire gameState.Board gameState.NextMove
        }

