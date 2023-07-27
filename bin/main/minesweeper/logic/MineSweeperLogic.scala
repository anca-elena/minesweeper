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
      var countDiscovered = 0
      gameState.gameBoard.foreach(line => line.foreach
        (tile =>
          if(!tileIsCovered(tile)) {
            countDiscovered += 1
          }
        )
      )

      if(countDiscovered == WIDTH * HEIGHT - NUM_BOMBS) {
        return true
      }
      false
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

  def placeOrRemoveFlag(x: Int, y: Int) : Unit = {
    if(!gameOver && !gameWon) {
      gameState = gameState.placeOrRemoveFlag(x, y)
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
  val DefaultWidth: Int = 20
  val NrTopInvisibleLines: Int = 4
  val DefaultVisibleHeight: Int = 20
  val DefaultHeight: Int = DefaultVisibleHeight + NrTopInvisibleLines
  val DefaultDims: Dimensions = Dimensions(width = DefaultWidth, height = DefaultHeight)

  def apply() = new MineSweeperLogic(DefaultDims, createBoard(DefaultDims))

  def initBoard(gridDims: Dimensions): Array[Array[(TileType, Boolean, Boolean, Boolean, Int)]]  = {
    val board = Array.ofDim[(TileType, Boolean, Boolean, Boolean, Int)](gridDims.width, gridDims.height)
    for (x <- 0 until gridDims.width; y <- 0 until gridDims.height) {
      board(x)(y) = (DiscoveredTile, IS_COVERED, !HAS_FLAG, !IS_START, INIT_COUNT)
    }
    board
  }

  def addBombs(board: Array[Array[(TileType, Boolean, Boolean, Boolean, Int)]], gridDims: Dimensions): Array[Array[(TileType, Boolean, Boolean, Boolean, Int)]] = {
    val newBoard = board
    var randomX = Random.nextInt(gridDims.width - 1)
    var randomY = Random.nextInt(gridDims.width)
    for (i <- 0 until NUM_BOMBS + 1) {
      while (newBoard(randomX)(randomY)._1 == Bomb) {
        randomX = Random.nextInt(gridDims.width - 1)
        randomY = Random.nextInt(gridDims.width)
      }
      if (i != NUM_BOMBS) {
        newBoard(randomX)(randomY) = (Bomb, IS_COVERED, !HAS_FLAG, !IS_START, INIT_COUNT)
      }
    }
    // Instead of placing one more bomb, initialize the safe tile X
    newBoard(randomX)(randomY) = (DiscoveredTile, IS_COVERED, !HAS_FLAG, IS_START, INIT_COUNT)
    newBoard
  }

  def initNumberTiles(board: Array[Array[(TileType, Boolean, Boolean, Boolean, Int)]]) : Array[Array[(TileType, Boolean, Boolean, Boolean, Int)]] = {
    val newBoard = board
    for (x <- board.indices; y <- board.indices) {
      if (newBoard(x)(y)._1 != Bomb) {
        val bombCount = countBombs(x, y, newBoard)
        if (bombCount > 0) {
          newBoard(x)(y) = (NumberTile, IS_COVERED, !HAS_FLAG, newBoard(x)(y)._4, bombCount)
        }
      }
    }
    newBoard
  }

  def createBoard(gridDims: Dimensions) : GameState = {
    var board = initBoard(gridDims)
    board = addBombs(board, gridDims)
    board = initNumberTiles(board)

    GameState(board, gameOver = false)
  }

  private def countBombs(x: Int, y: Int, board: Array[Array[(TileType, Boolean, Boolean, Boolean, Int)]]): Int = {
    def withinBounds(x: Int, y: Int): Boolean = {
      x >= 0 && y >= 0 && x < board.length - 1 && y < board.length
    }

    var bombCounter = 0
    for (neighborX <- -1 to 1; neighborY <- -1 to 1) {
      if (withinBounds(x + neighborX, y + neighborY) && board(x + neighborX)(y + neighborY)._1 == Bomb) {
        bombCounter += 1
      }
    }
    bombCounter
  }
}