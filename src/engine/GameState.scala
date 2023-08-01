package engine

import minesweeper.logic._

case class GameState(gameBoard : Array[Array[(TileType, Boolean, Boolean, Int)]], gameOver: Boolean){

  val IS_COVERED = true
  val HAS_FLAG = true
  val IS_START = true
  val WIDTH = 19
  val HEIGHT = 20

  def discoverTile(x: Int, y: Int, board: Array[Array[(TileType, Boolean, Boolean, Int)]], pressedNumber : Boolean) : GameState = {
    if(withinBounds(x, y)) {
      val newBoard = board
      val tileType = board(x)(y)._1
      val bombCount = board(x)(y)._5

      if(tileType == NumberTile && pressedNumber) {
        if(countFlags(x, y) == bombCount) {
          if (allBombsHaveFlags(x, y, board)) {
            for (i <- -1 to 1; j <- -1 to 1) {
              if (withinBounds(x + i, y + j) && (i != 0 || j != 0)) {
                discoverTile(x + i, y + j, newBoard, pressedNumber = false)
              }
            }
          }
          else {
            println("Not all bombs were flagged (??)")
            return GameState(revealAllBombs(newBoard), gameOver = true)
          }
        }
      }

      else if (isCovered(x, y) && !hasFlag(x, y)) {
        if (tileType == Bomb) {
          println("Discovered tile was a bomb")
          newBoard(x)(y) = (RedBomb, !IS_COVERED, !HAS_FLAG, !IS_START, bombCount)
          return GameState(revealAllBombs(newBoard), gameOver = true)
        }
        else {
          newBoard(x)(y) = (tileType, !IS_COVERED, !HAS_FLAG, !IS_START, bombCount)

          if (bombCount == 0) {
            for (i <- -1 to 1; j <- -1 to 1) {
              if (withinBounds(x + i, y + j) && (j != 0 || i != 0)) {
                val newTile = newBoard(x + i)(y + j)._1
                if(isCovered(x + i, y + j) && !hasFlag(x + i, y + j) && newTile != Bomb) {
                  discoverTile(x + i, y + j, newBoard, pressedNumber = false)
                }
              }
            }
          }
          return GameState(newBoard, gameOver = false)
        }
      }
    }
    GameState(gameBoard, gameOver = false)
  }

  def flagTile(x: Int, y: Int) : GameState = {
    val newBoard = gameBoard
    val tileType = gameBoard(x)(y)._1
    val bombCount = gameBoard(x)(y)._5

    if(withinBounds(x, y) && isCovered(x, y) && !isStart(x, y))
      newBoard(x)(y) = (tileType, IS_COVERED, !hasFlag(x, y), !IS_START, bombCount)

    GameState(newBoard, gameOver = false)
  }

  private def revealAllBombs(board: Array[Array[(TileType, Boolean, Boolean, Int)]])
                             : Array[Array[(TileType, Boolean, Boolean, Int)]] = {
    board.indices.map{ x=>
      board(0).indices.map{ y =>
        val tileType = board(x)(y)._1
        if (tileType == Bomb && !hasFlag(x, y))
          (Bomb, !IS_COVERED, !HAS_FLAG, !IS_START, 0)
        else if(tileType != Bomb && hasFlag(x, y))
          (WrongFlag, !IS_COVERED, !HAS_FLAG, !IS_START, 0)
        else
          board(x)(y)
      }.toArray
    }.toArray
  }

  private def countFlags(x: Int, y: Int) : Int = {
    (-1 to 1).flatMap(dx => (-1 to 1).map(dy => (x + dx, y + dy))).count {
      case (neighborX, neighborY) =>
        withinBounds(neighborX, neighborY) && hasFlag(neighborX, neighborY)
    }
  }

  private def allBombsHaveFlags(x: Int, y: Int, board: Array[Array[(TileType, Boolean, Boolean, Int)]]): Boolean = {
    (-1 to 1).flatMap(dx => (-1 to 1).map(dy => (x + dx, y + dy))).forall {
      case (neighborX, neighborY) =>
        !withinBounds(neighborX, neighborY) || board(neighborX)(neighborY)._1 != Bomb || hasFlag(neighborX, neighborY)
    }
  }

  private def isCovered(x: Int, y: Int) : Boolean = {
    gameBoard(x)(y)._2
  }

  private def hasFlag(x: Int, y: Int) : Boolean = {
    gameBoard(x)(y)._3
  }

  private def isStart(x: Int, y: Int) : Boolean = {
    gameBoard(x)(y)._4
  }

  private def withinBounds(x: Int, y: Int) : Boolean = {
    x >= 0 && y >= 0 && x < WIDTH && y < HEIGHT
  }
}
