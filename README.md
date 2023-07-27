# Hello and welcome to Classic Minesweeper!

### How to run the game:

First, you're going to need to have IntelliJ installed along with the Scala plugin and JDK 11. Open the project in IntelliJ and: 
- Simply select the "MineSweeper" configuration in the top-right drop-down menu and click run.
- If it doesn't work, try going into Edit Configurations and changing the Java version of "MineSweeper" to the first option.
- If that doesn't work either, navigate to src > minesweeper > game > MineSweeperGame and choose "Current File" from the menu.

If neither of those options work just let me know and I will ~~probably cry~~ try to figure it out!

### How to interact with the game:

- To reveal a tile, left-click on it.
- To flag or un-flag a tile, right-click.
- You can also left-click on number tiles. If the number on the tile and the number of flags you've placed around it are
the same, clicking it will auto-reveal all the un-flagged tiles (and possibly make you lose if you messed up).
- If you've lost, won, or simply want to restart the game, left-click the little smiley face at the top.

### Basic functionality:

When running the game you will encounter a 19 by 20 grid with a tile marked with an X.
You're free to start with any tile, but the tile marked with X is guaranteed to result in the biggest cleared area.
The top left corner shows the number of bombs you have left (can go into the negatives if you place
more flags than there are bombs!).
The top right corner shows how much time you've spent on a game (up to 999 seconds).
You win when you uncovered every non-bomb tile. You lose if you reveal a bomb.

Lastly, a screenshot from the game is stored in the `game_screenshots` folder. 
Here's a video containing gameplay: https://video.vu.nl/media/Minesweeper+OOFP+2022+Bonus+-+Anca+Strulea/1_0ugl21l6

Have fun!
