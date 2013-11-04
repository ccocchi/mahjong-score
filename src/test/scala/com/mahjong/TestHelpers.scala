package com.mahjong

import Tile.Suit._
import Wind.Direction._

object PongFactory {

  def ofSuited(value: Int = 1, isKong: Boolean = false): PongLike = {
    val tiles = Seq.fill(3)(Suited(Bamboo, value))
    if (isKong) new Kong(tiles) else new Pong(tiles)
  }

  def ofWind(direction: Direction): PongLike = {
    val tiles = Seq.fill(3)(Wind(direction))
    new Pong(tiles)
  }
}

object PairFactory {
  def ofSuited(value: Int = 1, suit: Suit): Pair = {
    new Pair(Seq.fill(2)(Suited(suit, value)))
  }
}