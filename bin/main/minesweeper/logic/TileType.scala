package minesweeper.logic

abstract class TileType
case object CoveredTile extends TileType
case object DiscoveredTile extends TileType
case object Flag extends TileType
case object WrongFlag extends TileType
case object Bomb extends TileType
case object RedBomb extends TileType
case object StartHere extends TileType
case object NumberTile extends TileType

