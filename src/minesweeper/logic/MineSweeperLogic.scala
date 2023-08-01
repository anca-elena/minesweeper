package minesweeper.logic

import engine.GameState

import scala.util.Random

class MineSweeperLogic(val gridDims: Dimensions,
                       val initialState: GameState) {

    var gameState: GameState = initialState
    val WIDTH: Int = MineSweeperLogic.DefaultWidth - 1
    val HEIGHT: Int = MineSweeperLogic.DefaultWidth
    val NUM_BOMBS: Int = MineSweeperLogic.NUM_BOMBS

    def gameWon : Boolean = {
      val countDiscovered = gameState.gameBoard.flatten.count(tile => !tileIsCovered(tile))
      countDiscovered == WIDTH * HEIGHT - NUM_BOMBS
    }

    def gameOver : Boolean = {
      gameState.gameOver
    }

    def getTileType(x: Int, y: Int) : TileType = {
      if(isStart(x, y)) {
        return StartHere
      }
      else if(isCovered(x, y) && !hasFlag(x, y)) {
        return CoveredTile
      }
      else if (hasFlag(x, y)){
        return Flag
      }
      gameState.gameBoard(x)(y)._1
  }

  def discoverTile(x: Int, y: Int) : Unit = {
    if(!gameOver && !gameWon) {
      if(gameState.gameBoard(x)(y)._1 == NumberTile && !isCovered(x, y)) {
        gameState = gameState.discoverTile(x, y, gameState.gameBoard, pressedNumber = true)
      }
      else {
        gameState = gameState.discoverTile(x, y, gameState.gameBoard, pressedNumber = false)
      }
    }
  }

  def flag(x: Int, y: Int) : Unit = {
    if(!gameOver && !gameWon) {
      gameState = gameState.flagTile(x, y)
    }
  }

  private def isCovered(x: Int, y: Int): Boolean = {
    gameState.gameBoard(x)(y)._2
  }

  private def tileIsCovered(tile: (TileType, Boolean, Boolean, Boolean, Int)) : Boolean = {
    tile._2
  }

  private def hasFlag(x: Int, y: Int): Boolean = {
    gameState.gameBoard(x)(y)._3
  }

  private def isStart(x: Int, y: Int): Boolean = {
    gameState.gameBoard(x)(y)._4
  }
}

object MineSweeperLogic {
  val DrawSizeFactor = 1.8

  val NUM_BOMBS = 70
  val IS_COVERED = true
  val HAS_FLAG = true
  val IS_START = true
  val INIT_COUNT = 0
  val DefaultWidth: Int = 19
  val NrTopInvisibleLines: Int = 4
  val DefaultVisibleHeight: Int = 20
  val DefaultHeight: Int = DefaultVisibleHeight + NrTopInvisibleLines
  val DefaultDims: Dimensions = Dimensions(width = DefaultWidth, height = DefaultHeight)

  def apply() = new MineSweeperLogic(DefaultDims, createBoard(DefaultDims))

  def initBoard(gridDims: Dimensions): Array[Array[(TileType, Boolean, Boolean, Boolean, Int)]]  = {
    Array.fill(gridDims.width, gridDims.height)(DiscoveredTile, IS_COVERED, !HAS_FLAG, !IS_START, INIT_COUNT)
  }

  //TODO: add bombs after the player clicked a tile, so that they never click on a bomb first try
  def addBombs(board: Array[Array[(TileType, Boolean, Boolean, Boolean, Int)]])
               : Array[Array[(TileType, Boolean, Boolean, Boolean, Int)]] = {

    val newBoard = board
    val bombCoordinates = Random.shuffle(for {x <- 0 until DefaultWidth - 1;
                                              y <- 0 until DefaultVisibleHeight}
                                              yield (x, y)).filter(_ != (0, 0))

    for (i <- 0 until NUM_BOMBS) {
      newBoard(bombCoordinates(i)._1)(bombCoordinates(i)._2) = (Bomb, IS_COVERED, !HAS_FLAG, !IS_START, INIT_COUNT)
    }
    newBoard
  }
  def initNumberTiles(board: Array[Array[(TileType, Boolean, Boolean, Boolean, Int)]])
                      : Array[Array[(TileType, Boolean, Boolean, Boolean, Int)]] = {

    board.indices.map( x =>
      board(x).indices.map( y=>
        if(board(x)(y)._1 != Bomb) {
          if(countBombs(x, y, board) > 0)
            (NumberTile, IS_COVERED, !HAS_FLAG, board(x)(y)._4, countBombs(x, y, board))
          else
            board(x)(y)
        }
        else {
          board(x)(y)
        }
      ).toArray
    ).toArray
}


  def createBoard(gridDims: Dimensions) : GameState = {
    var board = initBoard(gridDims)
    board = addBombs(board)
    board = initNumberTiles(board)

    GameState(board, gameOver = false)
  }

  private def countBombs(x: Int, y: Int, board: Array[Array[(TileType, Boolean, Boolean, Boolean, Int)]]): Int = {
    def withinBounds(x: Int, y: Int): Boolean = {
      x >= 0 && y >= 0 && x < board.length - 1 && y < board.length
    }

    (-1 to 1).flatMap(dx =>
      (-1 to 1).map(dy =>
        (x + dx, y + dy))).count {
          case(neighborX, neighborY) =>
            withinBounds(neighborX, neighborY) && board(neighborX)(neighborY)._1 == Bomb
        }
  }
}