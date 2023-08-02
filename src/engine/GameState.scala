package engine

import minesweeper.logic._

case class GameState(gameBoard : Array[Array[Tile]], gameOver: Boolean){

  val IS_COVERED = true
  val HAS_FLAG = true
  val WIDTH: Int = MineSweeperLogic.DefaultWidth
  val HEIGHT: Int = MineSweeperLogic.DefaultVisibleHeight

  def discoverTile(x: Int, y: Int, board: Array[Array[Tile]], pressedNumber : Boolean) : GameState = {
    if(withinBounds(x, y)) {
      val newBoard = board
      val tile = newBoard(x)(y)
      
      if(tile._tileType == NumberTile && pressedNumber) {
        if(countFlags(x, y) == tile._bombCount) {
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

      else if (tile._isCovered && !tile._hasFlag) {
        if (tile._tileType == Bomb) {
          println("Discovered tile was a bomb")
          newBoard(x)(y) = new Tile(RedBomb, tile._bombCount, !IS_COVERED, !HAS_FLAG)
          return GameState(revealAllBombs(newBoard), gameOver = true)
        }
        else {
          newBoard(x)(y) = new Tile(tile._tileType, tile._bombCount, !IS_COVERED, !HAS_FLAG)

          if (tile._bombCount == 0) {
            for (i <- -1 to 1; j <- -1 to 1) {
              if (withinBounds(x + i, y + j) && (j != 0 || i != 0)) {
                val newTile = newBoard(x + i)(y + j)
                if(newTile._isCovered && !newTile._hasFlag && newTile._tileType != Bomb) {
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
    val tile = gameBoard(x)(y)

    if(withinBounds(x, y) && tile._isCovered)
      newBoard(x)(y) = new Tile(tile._tileType, tile._bombCount, IS_COVERED, !tile._hasFlag)

    GameState(newBoard, gameOver = false)
  }

  private def revealAllBombs(board: Array[Array[Tile]])
                             : Array[Array[Tile]] = {
    board.indices.map{ x=>
      board(0).indices.map{ y =>
        val tile = board(x)(y)
        if (tile._tileType == Bomb && !tile._hasFlag)
          new Tile(Bomb, 0, !IS_COVERED, !HAS_FLAG)
        else if(tile._tileType != Bomb && tile._hasFlag)
          new Tile(WrongFlag, 0, !IS_COVERED, !HAS_FLAG)
        else
          tile
      }.toArray
    }.toArray
  }

  private def countFlags(x: Int, y: Int) : Int = {
    (-1 to 1).flatMap(dx => (-1 to 1).map(dy => (x + dx, y + dy))).count {
      case (neighborX, neighborY) =>
        withinBounds(neighborX, neighborY) && gameBoard(neighborX)(neighborY)._hasFlag
    }
  }

  private def allBombsHaveFlags(x: Int, y: Int, board: Array[Array[Tile]]): Boolean = {
    (-1 to 1).flatMap(dx => (-1 to 1).map(dy => (x + dx, y + dy))).forall {
      case (neighborX, neighborY) =>
          !withinBounds(neighborX, neighborY) ||
          board(neighborX)(neighborY)._tileType != Bomb ||
          board(neighborX)(neighborY)._hasFlag
    }
  }

  private def withinBounds(x: Int, y: Int) : Boolean = {
    x >= 0 && y >= 0 && x < WIDTH - 1 && y < HEIGHT
  }
}
