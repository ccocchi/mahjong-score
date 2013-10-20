package com.mahjong.hands

import com.mahjong.{Suited, Dragon, Hand, ScoringHand, Tile}
import com.mahjong.points.Points8

class Miscellaneous {
}

class Symmetry extends ScoringHand with Points8 {
  import Tile.Color.White
  import Tile.Suit.{Dot, Bamboo}

  val authorizedTiles = Seq(
    Dragon(White),
    Suited(Dot, 1),
    Suited(Dot, 2),
    Suited(Dot, 3),
    Suited(Dot, 4),
    Suited(Dot, 5),
    Suited(Dot, 8),
    Suited(Dot, 9),
    Suited(Bamboo, 2),
    Suited(Bamboo, 4),
    Suited(Bamboo, 5),
    Suited(Bamboo, 6),
    Suited(Bamboo, 8),
    Suited(Bamboo, 9)
  )

  protected def isCompletedBy(hand: Hand) = hand.tiles.forall(authorizedTiles contains)
}
