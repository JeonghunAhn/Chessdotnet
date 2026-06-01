module Chess.Board

open Chess.Types

let boardSize = 8
let squareSize = 90.0
let canvasSize = squareSize * float boardSize

let isInside square =
    square.File >= 0
    && square.File < boardSize
    && square.Rank >= 0
    && square.Rank < boardSize

let squareIndex square =
    square.Rank * boardSize + square.File

let pieceAt (board: Board) square =
    if isInside square then
        board.[squareIndex square]
    else
        None

let private backRank color =
    [| { Color = color; Kind = Rook }
       { Color = color; Kind = Knight }
       { Color = color; Kind = Bishop }
       { Color = color; Kind = Queen }
       { Color = color; Kind = King }
       { Color = color; Kind = Bishop }
       { Color = color; Kind = Knight }
       { Color = color; Kind = Rook } |]

let initialBoard () =
    let board = Array.create<Piece option> (boardSize * boardSize) None

    for file in 0 .. boardSize - 1 do
        board.[squareIndex { File = file; Rank = 0 }] <- Some (backRank Black).[file]
        board.[squareIndex { File = file; Rank = 1 }] <- Some { Color = Black; Kind = Pawn }
        board.[squareIndex { File = file; Rank = 6 }] <- Some { Color = White; Kind = Pawn }
        board.[squareIndex { File = file; Rank = 7 }] <- Some (backRank White).[file]

    board

let initialModel () =
    { Board = initialBoard ()
      SelectedSquare = None
      LegalMoves = []
      SideToMove = White
      CastlingRights =
        { WhiteKingSide = true
          WhiteQueenSide = true
          BlackKingSide = true
          BlackQueenSide = true }
      EnPassantTarget = None
      PendingPromotion = None
      GameResult = None
      Clock =
        { WhiteRemainingMs = 5.0 * 60.0 * 1000.0
          BlackRemainingMs = 5.0 * 60.0 * 1000.0
          LastTickMs = None
          Started = false }
      Scene = MainMenu
      History = []
      HalfmoveClock = 0
      PositionHistory = []
      MoveHistory = []
      ComputerThinking = false
      LastSearch = None
      GameMode = LocalGame
      TimeControlMinutes = 5 }

let setPiece square piece (board: Board) =
    let next = Array.copy board
    next.[squareIndex square] <- piece
    next
