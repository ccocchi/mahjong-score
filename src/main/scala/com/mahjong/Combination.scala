package com.mahjong

abstract class Combination {
  val tilesNumber: Int
  val tiles: Seq[Tile]

  assert(tilesNumber == tiles.size)

  protected val hidden: Boolean = true

  lazy val value: Option[Int] = tiles.head.value
}

class SingleTile(val tile: Tile) extends Combination {
  val tilesNumber = 1
  val tiles = Seq(tile)
}

class Pair(val tiles: Seq[Tile]) extends Combination {
  val tilesNumber = 2
}

class Pong(val tiles: Seq[Tile]) extends Combination {
  val tilesNumber = 3
}

class Kong(val tiles: Seq[Tile]) extends Combination {
  val tilesNumber = 3
}

class Chow(val tiles: Seq[Tile]) extends Combination {
  val tilesNumber = 3
}