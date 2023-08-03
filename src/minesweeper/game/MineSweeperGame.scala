package minesweeper.game

import engine.GameBase
import engine.graphics.{Point, Rectangle}
import minesweeper.game.MineSweeperGame.{HeightCellInPixels, WidthCellInPixels}
import minesweeper.logic._
import processing.core.{PApplet, PConstants, PImage}

class MineSweeperGame extends GameBase {

  val GRID_BUFFER = 16

  var gameLogic: MineSweeperLogic = MineSweeperLogic()
  val gridDims: Dimensions = gameLogic.gridDims
  val pxWidth: Int = (WidthCellInPixels * gridDims.width).ceil.toInt
  val pxHeight: Int = (HeightCellInPixels * gridDims.height).ceil.toInt
  val cellWidth: Float = (pxWidth / gridDims.width).toFloat

  var timer : Int = millis()
  var secondsElapsed: Int = 0

  // Basic tiles
  val tiles : Array[PImage] = new Array[PImage](6)
  val tilePaths: Array[String] = Array(
    "src/engine/graphics/sprites/coveredTile.png",
    "src/engine/graphics/sprites/discoveredTile.png",
    "src/engine/graphics/sprites/bomb.png",
    "src/engine/graphics/sprites/redBomb.png",
    "src/engine/graphics/sprites/flag.png",
    "src/engine/graphics/sprites/wrongFlag.png")

  // Number tiles
  val numbers : Array[PImage] = new Array[PImage](8)
  val numberPaths : List[String] = List(
    "src/engine/graphics/sprites/one.png",
    "src/engine/graphics/sprites/two.png",
    "src/engine/graphics/sprites/three.png",
    "src/engine/graphics/sprites/four.png",
    "src/engine/graphics/sprites/five.png",
    "src/engine/graphics/sprites/six.png",
    "src/engine/graphics/sprites/seven.png",
    "src/engine/graphics/sprites/eight.png")

  // Smileys
  val smileys : Array[PImage] = new Array[PImage](3)
  val smileyPaths : List[String] = List(
    "src/engine/graphics/sprites/start.png",
    "src/engine/graphics/sprites/loser.png",
    "src/engine/graphics/sprites/winner.png")

  // 20 x 20 background
//  var backgroundImage: PImage = createImage(0, 0, 0)

  override def draw(): Unit = {
    drawGrid()
  }

  // Load all images + font for bomb count and timer
  override def setup(): Unit = {
    for(i <- tiles.indices) {
      tiles(i) = loadImage(tilePaths(i))
      tiles(i).resize(cellWidth.toInt, cellWidth.toInt)
    }

    for(i <- numbers.indices) {
      numbers(i) = loadImage(numberPaths(i))
      numbers(i).resize(cellWidth.toInt, cellWidth.toInt)
    }

    for(i <- smileys.indices) {
      smileys(i) = loadImage(smileyPaths(i))
      smileys(i).resize((cellWidth * 2).toInt, (cellWidth * 2).toInt)
    }

    val backgroundImage = loadImage("src/engine/graphics/sprites/20x20background.png")
    backgroundImage.resize(pxWidth, pxHeight)

    val font = createFont("src/engine/graphics/sprites/ConnectionIii-Rj3W.otf", 45)
    textFont(font)
    background(backgroundImage)
  }

  def drawGrid(): Unit = {
    updateTimer()
    drawBombCountAndTime()
    drawSmiley()
    drawTiles()
  }

  override def settings(): Unit = {
    pixelDensity(displayDensity())
    size(pxWidth, pxHeight)
  }

  override def mouseClicked() : Unit = {
    val LEFT = 37
    val RIGHT = 39

    val tileX = getTileX(mouseX)
    val tileY = getTileY(mouseY)

    if(mouseButton == LEFT && mouseOverSmiley(mouseX, mouseY)) {
      secondsElapsed = 0
      gameLogic.gameState = MineSweeperLogic.createBoard(gridDims)
    }
    else if(mouseButton == LEFT) {
      if(tileX >= 0 && tileY >= 0) {
        gameLogic.discoverTile(tileX, tileY)
      }
    }
    else if(mouseButton == RIGHT) {
      gameLogic.flagTile(tileX, tileY)
    }
    redraw()
  }

  private def updateTimer(): Unit = {
    if (millis() - timer >= 1000 && !gameLogic.gameWon && !gameLogic.gameState.gameOver) {
      if (secondsElapsed < 999) {
        secondsElapsed += 1
      }
      timer = millis()
    }
  }

  private def drawBombCountAndTime(): Unit = {
    val leftBlackScreenX = 2 + GRID_BUFFER.toFloat
    val rightBlackScreenX = pxWidth - 7 * (GRID_BUFFER.toFloat + 1)
    val blackScreenY = 2 + GRID_BUFFER.toFloat
    val blackScreenHeight = (MineSweeperLogic.NrTopInvisibleLines - 1) * GRID_BUFFER.toFloat
    val blackScreenWidth = blackScreenHeight * 2
    val bombCountX = (1.5 * GRID_BUFFER).toFloat
    val secondsX = pxWidth - 7 * GRID_BUFFER.toFloat
    val textY = 10 + ((MineSweeperLogic.NrTopInvisibleLines - 1) * GRID_BUFFER).toFloat

    fill(0)
    var blackScreen = Rectangle(Point(leftBlackScreenX, blackScreenY), blackScreenWidth, blackScreenHeight)
    drawRectangle(blackScreen)
    blackScreen = Rectangle(Point(rightBlackScreenX, blackScreenY), blackScreenWidth, blackScreenHeight)
    drawRectangle(blackScreen)

    fill(255, 0, 0)
    drawText(countBombsLeft().toString, Point(bombCountX, textY), withShadow = false)
    drawText(secondsElapsed.toString, Point(secondsX, textY), withShadow = false)
  }

  private def drawSmiley(): Unit = {
    val index = gameLogic match {
      case state if state.gameState.gameOver => 1
      case state if state.gameWon => 2
      case _ => 0
    }

    image(smileys(index), (pxWidth / 2 - GRID_BUFFER).toFloat, GRID_BUFFER.toFloat)
  }

  private def drawTiles(): Unit = {
    for (i <- 0 until gridDims.width - 1;
         j <- MineSweeperLogic.NrTopInvisibleLines - 1 until gridDims.height - 1) {

      val tile = gameLogic.getTileType(i, j - MineSweeperLogic.NrTopInvisibleLines + 1)
      val pixelRow = getPixelCoord(i).toFloat
      val pixelCol = getPixelCoord(j).toFloat

      tile match {
        case CoveredTile => image(tiles(0), pixelRow, pixelCol)
        case DiscoveredTile => image(tiles(1), pixelRow, pixelCol)
        case Bomb => image(tiles(2), pixelRow, pixelCol)
        case RedBomb => image(tiles(3), pixelRow, pixelCol)
        case Flag => image(tiles(4), pixelRow, pixelCol)
        case WrongFlag => image(tiles(5), pixelRow, pixelCol)
        case NumberTile =>
          val number = gameLogic.gameState.gameBoard(i)(j - MineSweeperLogic.NrTopInvisibleLines + 1)._bombCount - 1
          image(numbers(number), pixelRow, pixelCol)
      }
    }
  }

  // Convert pixel coordinates to Tile coordinates and vice-versa
  private def getTileX(x: Int): Int = {
    ((x - GRID_BUFFER) / cellWidth).toInt
  }

  private def getTileY(y: Int): Int = {
    ((y - GRID_BUFFER) / cellWidth - MineSweeperLogic.NrTopInvisibleLines + 1).toInt
  }

  private def getPixelCoord(coord: Int): Int = {
    (coord * cellWidth + GRID_BUFFER).toInt
  }

  private def mouseOverSmiley(mouseX: Int, mouseY: Int): Boolean = {
    mouseX >= pxWidth / 2 - GRID_BUFFER && mouseX <= pxWidth / 2 + GRID_BUFFER * 2 &&
      mouseY >= GRID_BUFFER && mouseY <= GRID_BUFFER * 3
  }

  private def countBombsLeft() : Int = {
    var countFlagged = 0
    for (i <- gameLogic.gameState.gameBoard.indices; j <- gameLogic.gameState.gameBoard.indices) {
      if (gameLogic.gameState.gameBoard(i)(j)._hasFlag) {
        countFlagged += 1
      }
    }
    MineSweeperLogic.NUM_BOMBS - countFlagged
  }

}

object MineSweeperGame extends GameBase {
  val WidthCellInPixels: Double = 15 * MineSweeperLogic.DrawSizeFactor
  val HeightCellInPixels: Double = WidthCellInPixels

  def main(args: Array[String]): Unit = {
    PApplet.main("minesweeper.game.MineSweeperGame")

  }
}
