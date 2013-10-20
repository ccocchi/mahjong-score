package com.mahjong

import com.mahjong.Tile.Suit.Suit

abstract class Combination {
  val tilesNumber: Int
  val tiles: Seq[Tile]

  protected val concealed: Boolean = true

  lazy val value: Option[Int] = tiles.head.value
  lazy val suit: Option[Suit] = tiles.head match {
    case t: Suited => t.suit
    case _ => None
  }

  def isHidden: Boolean = concealed
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

  val getValue: Int = value.get
  override def isPongLike = true
}

class Pong(val tiles: Seq[Tile]) extends PongLike {
  override def isPong = true
}

class Kong(val tiles: Seq[Tile]) extends PongLike {
  override def isKong = true
}

object Chow {
  def apply(tiles: Seq[Tile]) = {
    assert(tiles.forall(_.value.isDefined))
    new Chow(tiles.sortBy(_.value.get))
  }

  def apply(start: Int, suit: Suit, hidden: Boolean = false) = {
    val end = start + 2
    val tiles = start.to(end).map { i =>
      Suited(suit, i)
    }
    new Chow(tiles) {
      override protected val concealed = hidden
    }
  }
}

class Chow(val tiles: Seq[Tile]) extends Combination {
  val tilesNumber = 3

  def getValue: Int = value.get
  def getSuit: Suit = suit.get

  override def ==(c: Chow) = this.valuesSum == c.valuesSum

  protected def valuesSum = tiles.flatMap(_.value).sum
}