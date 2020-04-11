namespace FsChess

module Domain =

    type Colour = | White | Black

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

    type Piece = { Player : Colour; Rank : Rank }
    type Board = Map<Square, Piece option>
    type ProposedMove = { From : Square; To : Square }
    type ProposedMoveWithKnownPiece = { SelectedPiece : Piece; From : Square; To : Square }
    type ValidatedMove = { Piece : Piece; From : Square; To : Square; CapturedPiece : Piece option }
    type GameStatus = | InProgress | WhiteInCheck | WhiteInCheckmate | BlackInCheck | BlackInCheckmate | Stalemate
    type GameState = { Board : Board; CurrentPlayer : Colour; Status : GameStatus; Message : string; MoveHistory : ValidatedMove list }

    type Vector =
        { X : int; Y : int }
            with
                static member MakeUnit distance =

                    let normalise n = if n > 0 then 1 elif n < 0 then -1 else 0

                    { X = normalise distance.X; Y = normalise distance.Y }

                static member FromSquare square =
                    let (col, row) = square
                    let colIdx = Column.List |> List.findIndex ((=) col)
                    let rowIdx = Row.List |> List.findIndex ((=) row)
                    { X = colIdx; Y =  rowIdx}

                static member ToSquare vector =
                    let colIdx = vector.X
                    let rowIdx = vector.Y
                    if colIdx < Column.List.Length && colIdx >= 0 && rowIdx < Column.List.Length && rowIdx >= 0
                    then Some (Column.List.[colIdx], Row.List.[rowIdx])
                    else None

                static member (+) (first, second) =
                    { X = first.X + second.X; Y =  first.Y + second.Y }

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

        let player =  White

        {   Board = board
            CurrentPlayer = player
            Status = InProgress
            Message = sprintf "%A to move" player
            MoveHistory = [] }

    let updateGameState (gameState : GameState) move : GameState =
        let p =
            match move.Piece.Rank with
            | Pawn NotMoved -> { move.Piece with Rank = Pawn Moved }
            | _ -> move.Piece

        let board = gameState.Board.Add(move.From, None).Add(move.To, Some p)

        let nextPlayer = match gameState.CurrentPlayer with | White -> Black | Black -> White

        {
            Board = board
            CurrentPlayer = nextPlayer
            Status = gameState.Status
            Message = sprintf "%A to move" nextPlayer
            MoveHistory = move :: gameState.MoveHistory
        }
