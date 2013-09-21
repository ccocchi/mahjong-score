package com.mahjong

import com.mahjong.Tile.Suit.Suit

abstract class Combination {
  val tilesNumber: Int
  val tiles: Seq[Tile]

  //assert(this.tilesNumber == tiles.size)

  protected val hidden: Boolean = true

  lazy val value: Option[Int] = tiles.head.value
  lazy val suit: Option[Suit] = tiles.head match {
    case t: Suited => Some(t.suit)
    case _ => None
  }

  def isHidden: Boolean = hidden
  def isPair: Boolean = false
  def isPong: Boolean = false
  def isPongLike: Boolean = false
  def isKong: Boolean = false
  def isChow: Boolean = tilesNumber > 1 && !isPair && !isPongLike

  def ==(c: Chow): Boolean = false
}

class SingleTile(val tile: Tile) extends Combination {
  val tilesNumber = 1
  val tiles = Seq(tile)
}

class Pair(val tiles: Seq[Tile]) extends Combination {
  val tilesNumber = 2

  override def isPair = true
}

abstract class PongLike extends Combination {
  val tilesNumber = 3

  override def isPongLike = true
}

class Pong(val tiles: Seq[Tile]) extends PongLike {
  override def isPong = true
}

class Kong(val tiles: Seq[Tile]) extends PongLike {
  override def isKong = true
}

class Chow(val tiles: Seq[Tile]) extends Combination {
  val tilesNumber = 3

  val getValue: Int = value.get

  override def ==(c: Chow) = this.valuesSum == c.valuesSum

  def compareShift(other: Chow, interval: Int = 1): Int = {
    this.getValue - other.getValue match {
      case i if i == -interval => -1
      case i if 1 == interval => 1
      case 0 => 0
      case _ => 2
    }
  }

  protected def valuesSum = tiles.flatMap(_.value).sum
}