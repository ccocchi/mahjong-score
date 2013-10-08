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
  val suit: Option[Suit]

  def ==(other: Suited): Boolean = false
  def ==(other: Wind): Boolean   = false
  def ==(other: Dragon): Boolean = false

  def isDragon: Boolean = false
  def isWind: Boolean = false
}

abstract class Honor extends Tile {
  val honor = true
  val value = None
  val suit  = None
}

object Suited {
  def apply(suit: Suit, value: Int) = new Suited(suit, value)
}

class Suited(val s: Suit, val n: Int) extends Tile {
  assert(n > 0 && n < 10)

  val honor = false
  override val terminal: Boolean = n == 1 || n == 9
  val value = Some(n)
  val suit  = Some(s)

  override def ==(other: Suited) = this.suit == other.suit && this.n == other.n
}

object Wind {
  object Direction extends Enumeration {
    type Direction = Value
    val North, South, East, West = Value
  }

  def apply(direction: Direction.Direction) = new Wind(direction)
}

import Wind.Direction.Direction

class Wind(val direction: Direction) extends Honor {
  override def ==(other: Wind) = this.direction == other.direction
  override def isWind = true
}

object Dragon {
  def apply(color: Color) = new Dragon(color)
}

class Dragon(val color: Color) extends Honor {
  override def ==(other: Dragon) = this.color == other.color
  override def isDragon = true
}


