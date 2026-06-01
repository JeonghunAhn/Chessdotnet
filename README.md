# Chess dotnet
Web-based chess built with F# / .NET 10
Used fable to show on web
Can play with F# builted evaluate engine or 2 player chess game.

## Getting Started

### Play
Can access by https://jeonghunahn.github.io/Chessdotnet/
### To run on local
### Prerequisites
- [.NET 10 SDK](https://dotnet.microsoft.com/download)  
  Verify with: `dotnet --version` (should show `10.x.x`)
- [node.js](https://nodejs.org/ko/download)
  Verify with: `node -v`

```bash
dotnet tool restore
dotnet restore
npm ci
dotnet fable
npm run dev
```

### Build
```bash
dotnet build
```
### Run
```bash
dotnet fable watch --verbose --run npx vite
```

## How to Play
- Select mode by gui button. (vs engine / 2 player)
- Board is displayed by web.
- Click your piece on your turn and click where to move.
- Can undo moves by button on right side.