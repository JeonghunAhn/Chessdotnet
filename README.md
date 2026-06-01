# Chess dotnet
Web-based chess built with F# / .NET 10
Used fable to show on web
Can play with F# builted evaluate engine or 2 player chess game.

## Getting Started

### To run on local
### Prerequisites
.NET10 SDK
Node.js

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