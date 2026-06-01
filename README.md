# Chess dotnet
Web-based chess built with F# / .NET 10
Used fable to show on web
Can play with F# builted evaluate engine or 2 player chess game.

## Getting Started

### Play on web
Can access by https://jeonghunahn.github.io/Chessdotnet/
### To run on local
### Prerequisites
- [.NET 10 SDK](https://dotnet.microsoft.com/download)  
  Verify with: `dotnet --version` (should show `10.x.x`)
- [node.js](https://nodejs.org/ko/download)
  Verify with: `node -version`

### Install Dependencies

```powershell
dotnet tool restore
dotnet restore
npm.cmd ci
```

```powershell
dotnet fable watch --run npm.cmd run dev
```

Then open:

```text
http://localhost:5173/Chessdotnet/
```

## How to Play
- Select mode by gui button. (vs engine / 2 player)
- Board is displayed by web.
- Click your piece on your turn and click where to move.
- Can undo moves by button on right side.
- Can move all legal moves filtered by move validation. (includes castling, en passant, and promotion)

### Winning and ending
- All game-ending rules implemented. (Checkmate, stalemate, timeout, threefold repetition, 50-move rule, and insufficient material detection)

## Project Structure

```text
Chessdotnet/
├── chessdotnet.fsproj       # F# project file and compilation order
├── index.html               # Browser entry page
├── package.json             # Vite scripts and JavaScript dependencies
├── vite.config.js           # Local server and GitHub Pages configuration
├── Types.fs                 # Shared chess and application types
├── Board.fs                 # Initial board and square operations
├── Rules.fs                 # Legal move generation and chess rules
├── Game.fs                  # Game state transitions, clocks, and undo
├── Evaluation.fs            # Board evaluation using piece-square tables
├── OpeningBook.fs           # Weighted opening move selection
├── Engine.fs                # Computer move search
├── EngineWorker.fs          # Web Worker entry point for background search
├── Ui.fs                    # Canvas layout and board perspective
├── Input.fs                 # Mouse input conversion and button hit detection
├── Render.fs                # Canvas rendering
├── Program.fs               # Browser initialization and event wiring
├── tests/                   # F# smoke tests
└── tools/PstTuner/          # Piece-square table tuning utility

## Key Types

```fsharp
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

type Move =
    { From: Square
      To: Square
      Promotion: PieceKind option }

type GameResult =
    | Checkmate of winner: Color
    | Stalemate
    | Timeout of winner: Color
    | Draw of reason: DrawReason
```

## Module Overview

| Module | Responsibility |
|--------|----------------|
| `Types` | Shared domain types for pieces, moves, clocks, engine messages, and game state |
| `Board` | Initial board setup, square indexing, and board updates |
| `Rules` | Legal move generation, check detection, castling, en passant, promotion, and draw helpers |
| `Game` | User actions, clocks, undo, move history, and game-over detection |
| `Evaluation` | Midgame and endgame board evaluation using tuned piece-square tables |
| `OpeningBook` | Random weighted selection from common opening continuations |
| `Engine` | Iterative deepening search with alpha-beta pruning, quiescence search, and a transposition table |
| `EngineWorker` | Runs the chess engine outside the browser UI thread |
| `Ui` | Canvas dimensions, buttons, and board orientation |
| `Input` | Converts mouse coordinates into board squares and UI actions |
| `Render` | Draws the board, pieces, highlights, clocks, menus, and result overlay |
| `Program` | Connects browser events, rendering, game updates, and the engine worker |

## LLM Usage
**Used LLM for** : Debuging and adding features after generate initial chess board structure, HTML and button ui. Making simple engine and tuned Piece-squre table by using 5,000 positions sampled from CC0 Lichess Stockfish evaluation dataset.

**Changed or reprompt** : I asked to make time limits before making game start button and dividing scenes, so time limit did not worked properly because it didn't know when to start white side player's time count. I changed the prompt to make start button and start time count.

**LLM was not able to do correctly** : While making simple engine, I asked LLM for example Piece-square table values, but it couldn't find proper values which i could use freely(such as CC0), so I changed to make own values by using evaluated datasets. I asked again to make own values by learning some CC0 datasets. And it couldn't optimize more to search moves more deeper(But it could be limited by my environment).