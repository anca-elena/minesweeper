package minesweeper.logic

class Tile(tileType: TileType,
           bombCount: Int,
           isCovered: Boolean,
           hasFlag: Boolean)
{
  var _tileType : TileType = tileType
  var _bombCount : Int = bombCount
  var _isCovered : Boolean = isCovered
  var _hasFlag : Boolean = hasFlag
}
