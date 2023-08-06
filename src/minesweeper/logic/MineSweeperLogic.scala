package minesweeper.logic

import engine.GameState

import scala.util.Random

class MineSweeperLogic(val gridDims: Dimensions, val initialState: GameState) {

  var gameState: GameState = initialState
  val WIDTH: Int = MineSweeperLogic.DefaultWidth
  val HEIGHT: Int = MineSweeperLogic.DefaultWidth
  val NUM_BOMBS: Int = MineSweeperLogic.NUM_BOMBS

  def gameWon : Boolean = {
    val countDiscovered = gameState.gameBoard.flatten.count(tile => !tile._isCovered)
    countDiscovered == WIDTH * HEIGHT - NUM_BOMBS - 1
  }

  def gameOver : Boolean = {
    gameState.gameOver
  }

  def gameStarted : Boolean = {
    gameState.gameStarted
  }

  def getTileType(x: Int, y: Int) : TileType = {
    val tile = gameState.gameBoard(x)(y)
    if(tile._isCovered && !tile._hasFlag) {
      return CoveredTile
    }
    else if (tile._hasFlag){
      return Flag
    }
    gameState.gameBoard(x)(y)._tileType
  }

  def discoverTile(x: Int, y: Int) : Unit = {
    val tile = gameState.gameBoard(x)(y)
    if(!gameOver && !gameWon) {
      if(tile._tileType == NumberTile && !tile._isCovered) {
        gameState = gameState.discoverTile(x, y, gameState.gameBoard, pressedNumber = true)
      }
      else {
        gameState = gameState.discoverTile(x, y, gameState.gameBoard, pressedNumber = false)
      }
    }
  }

  def flagTile(x: Int, y: Int) : Unit = {
    if(!gameOver && !gameWon) {
      gameState = gameState.flagTile(x, y)
    }
  }
}

object MineSweeperLogic {
  val DrawSizeFactor = 1.8

  val NUM_BOMBS = 70
  val IS_COVERED = true
  val HAS_FLAG = true
  val INIT_COUNT = 0
  val DefaultWidth: Int = 19
  val NrTopInvisibleLines: Int = 4
  val DefaultVisibleHeight: Int = 20
  val DefaultHeight: Int = DefaultVisibleHeight + NrTopInvisibleLines
  val DefaultDims: Dimensions = Dimensions(width = DefaultWidth, height = DefaultHeight)

  def apply() = new MineSweeperLogic(DefaultDims, createBoard(DefaultDims))

  def initBoard(gridDims: Dimensions): Array[Array[Tile]]  = {
    Array.fill(gridDims.width, gridDims.height)(new Tile(DiscoveredTile, INIT_COUNT, IS_COVERED, !HAS_FLAG))
  }

  //TODO: add bombs after the player clicked a tile, so that they never click on a bomb first try
  def addBombs(board: Array[Array[Tile]])
               : Array[Array[Tile]] = {

    val newBoard = board
    val bombCoordinates = Random.shuffle(for {x <- 0 until DefaultWidth - 1
                                              y <- 0 until DefaultVisibleHeight}
                                              yield (x, y)).filter(_ != (0, 0))

    for (i <- 0 until NUM_BOMBS) {
      newBoard(bombCoordinates(i)._1)(bombCoordinates(i)._2) = new Tile(Bomb, INIT_COUNT, IS_COVERED, !HAS_FLAG)
    }
    newBoard
  }
  def initNumberTiles(board: Array[Array[Tile]])
                      : Array[Array[Tile]] = {

    println(board.length, board(0).length)
    board.indices.map(x => board(x).indices.map(y=>
        if(board(x)(y)._tileType != Bomb) {
          if(countBombs(x, y, board) > 0)
            new Tile(NumberTile, countBombs(x, y, board), IS_COVERED, !HAS_FLAG)
          else
            board(x)(y)
        }
        else {
          new Tile(Bomb, 0, IS_COVERED, !HAS_FLAG)
        }
      ).toArray
    ).toArray
  }


  def createBoard(gridDims: Dimensions) : GameState = {
    var board = initBoard(gridDims)
    board = addBombs(board)
    board = initNumberTiles(board)

    GameState(board, gameOver = false, gameStarted = false)
  }

  private def countBombs(x: Int, y: Int, board: Array[Array[Tile]]): Int = {
    def withinBounds(x: Int, y: Int): Boolean = {
      x >= 0 && y >= 0 && x < DefaultWidth && y < DefaultVisibleHeight
    }

    (-1 to 1).flatMap(dx =>
      (-1 to 1).map(dy =>
        (x + dx, y + dy))).count {
          case(neighborX, neighborY) =>
            withinBounds(neighborX, neighborY) && board(neighborX)(neighborY)._tileType == Bomb
        }
  }
}