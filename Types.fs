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

type CastlingRights =
    { WhiteKingSide: bool
      WhiteQueenSide: bool
      BlackKingSide: bool
      BlackQueenSide: bool }

type PendingPromotion =
    { Moves: Move list }

type DrawReason =
    | ThreefoldRepetition
    | FiftyMoveRule
    | InsufficientMaterial

type GameResult =
    | Checkmate of winner: Color
    | Stalemate
    | Timeout of winner: Color
    | Draw of reason: DrawReason

type Clock =
    { WhiteRemainingMs: float
      BlackRemainingMs: float
      LastTickMs: float option
      Started: bool }

type SearchSummary =
    { UsedOpeningBook: bool
      Depth: int
      Nodes: int
      QuiescenceNodes: int
      Cutoffs: int
      TranspositionHits: int
      ElapsedMs: float }

type EngineRequest =
    { RequestId: int
      Board: Board
      SideToMove: Color
      CastlingRights: CastlingRights
      EnPassantTarget: Square option
      MoveHistory: Move list
      MaxDepth: int
      TimeLimitMs: float }

type EngineResponse =
    { RequestId: int
      Move: Move option
      Score: int
      Summary: SearchSummary }

type GameMode =
    | LocalGame
    | VsComputer of humanColor: Color

type Scene =
    | MainMenu
    | Playing

type GameSnapshot =
    { Board: Board
      SideToMove: Color
      CastlingRights: CastlingRights
      EnPassantTarget: Square option
      GameResult: GameResult option
      Clock: Clock
      HalfmoveClock: int
      PositionHistory: string list
      MoveHistory: Move list }

type Model =
    { Board: Board
      SelectedSquare: Square option
      LegalMoves: Move list
      SideToMove: Color
      CastlingRights: CastlingRights
      EnPassantTarget: Square option
      PendingPromotion: PendingPromotion option
      GameResult: GameResult option
      Clock: Clock
      Scene: Scene
      History: GameSnapshot list
      HalfmoveClock: int
      PositionHistory: string list
      MoveHistory: Move list
      ComputerThinking: bool
      LastSearch: SearchSummary option
      GameMode: GameMode
      TimeControlMinutes: int }
