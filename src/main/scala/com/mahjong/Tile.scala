package com.mahjong

object Tile {
  object Suit extends Enumeration {
    type Suit = Value
    val Dot, Bamboo, Character = Value
  }

  object Color extends Enumeration {
    type Color = Value
    val Red, Green, White = Value
  }
}

import Tile.Suit._
import Tile.Color._

sealed abstract class Tile {
  val honor: Boolean
  val terminal: Boolean = false
  val value: Option[Int]

  def ==(other: Suited): Boolean = false
  def ==(other: Wind): Boolean   = false
  def ==(other: Dragon): Boolean = false
}

abstract class Honor extends Tile {
  val honor = true
  val value = None
}

class Suited(val suit: Suit, val n: Int) extends Tile {
  assert(n > 0 && n < 10)

  val honor = false
  override val terminal: Boolean = n == 1 || n == 9
  val value = Some(n)

  override def ==(other: Suited) = this.suit == other.suit && this.n == other.n
}

class Wind(val direction: String) extends Honor {
  override def ==(other: Wind) = this.direction == other.direction
}

class Dragon(val color: Color) extends Honor {
  override def ==(other: Dragon) = this.color == other.color
}


