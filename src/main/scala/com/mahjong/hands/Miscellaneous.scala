package com.mahjong.hands

import com.mahjong.{Suited, Dragon, Hand, ScoringHand, Tile}
import com.mahjong.points.{Points4, Points2, Points1, Points8}

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

class OneMissingFamily extends ScoringHand with Points1 {
  protected def isCompletedBy(hand: Hand) = hand.tiles.flatMap(_.suit).groupBy(s => s).size == 2
}

class TerminalOrWindPung extends ScoringHand with Points1 {
  protected def isCompletedBy(hand: Hand) =
    hand.pongLikes.exists(p => p.getValue == 1 || p.getValue == 9 || p.tiles.exists(_.isWind))
}

class FourOfTheSame extends ScoringHand with Points2 {
  protected def isCompletedBy(hand: Hand) = {
    val f = hand.tiles.groupBy(_.value).mapValues(_.size)
    f.exists { case(v, size) => size == 4 && !hand.combinations.exists { c => c.isKong && c.value == v } }
  }
}

class HonorOrTerminalEverywhere extends ScoringHand with Points4 {
  protected def isCompletedBy(hand: Hand) = hand.combinations.forall { c =>
    c.tiles.exists { t =>
      val value = t.value.getOrElse(0)
      t.isDragon || t.isWind || value == 1 || value == 9
    }
  }
}