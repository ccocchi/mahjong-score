package com.mahjong

import Tile.Suit._

object PongFactory {
  def ofSuited(value: Int = 1, isKong: Boolean = false): PongLike = {
    val tiles = Seq.fill(3)(Suited(Bamboo, value))
    if (isKong) new Kong(tiles) else new Pong(tiles)
  }
}

object PairFactory {
  def ofSuited(value: Int = 1): Pair = {
    new Pair(Seq.fill(2)(Suited(Bamboo, value)))
  }
}