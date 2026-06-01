1.	To make a game with F#, we have to make or prepare classes and Interfaces which will act like game object, handle game status and render loop.

2.	After making the framework to act as game, I will implement chess.
Chess need
- board object
- piece object
- piece moving logic
- pawn promoting logic
- judge legal move
- judge winning or draw
- time control

3.	After completing Chess implementation, make simple chess engine to find optimal move.
- estimate current board position by recursively calculating the moves
- find best move by algorithms such as Alpha-Beta pruning
- If possible, try Reinforcement Learning to evaluate the move

## Added or changed requirements
- Make Scenes and buttons to start game.

  I needed this to divide playing with engine or human and also starting button is required to implement timer.(knowing when to start timer)
- Some HTML formats.

  Needed to display on web.
- PST (piece-square table) tables and evaluating PST values from datasets.

  Needed to implement simple engine.
- Add some visual convenience. (Emphasizing last move ..)

  To be more neater