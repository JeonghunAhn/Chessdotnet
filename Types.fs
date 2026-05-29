module Chess.Types

type Color =
    | White
    | Black

type PieceKind =
    | King
    | Queen
    | Rook
    | Bishop
    | Knight
    | Pawn

type Piece =
    { Color: Color
      Kind: PieceKind }

type Square =
    { File: int
      Rank: int }

type Board = Piece option array

type Move =
    { From: Square
      To: Square
      Promotion: PieceKind option }

type Model =
    { Board: Board
      SelectedSquare: Square option
      LegalTargets: Square list
      SideToMove: Color }
