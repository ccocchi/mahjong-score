package com.mahjong.hands

import com.mahjong.{Dragon, Suited, Hand, ScoringHand}
import com.mahjong.points._

class AllHonor extends ScoringHand with Points64 {
  protected def isCompletedBy(hand: Hand) = hand.tiles.forall(t => t.isDragon || t.isWind)
}

class AllTerminals extends ScoringHand with Points64 {
  protected def isCompletedBy(hand: Hand) = hand.tiles.forall(_.terminal)
}

class AllTerminalAndHonor extends ScoringHand with Points32 {
  protected def isCompletedBy(hand: Hand) = hand.tiles.forall(t => t.terminal || t.isDragon || t.isWind)
}

class AllConcealed extends ScoringHand with Points2 {
  protected def isCompletedBy(hand: Hand) = hand.combinations.forall(_.isHidden)
}

class AllRevealed extends ScoringHand with Points6 {
  protected def isCompletedBy(hand: Hand) = hand.combinations.forall(!_.isHidden)
}

class AllChow extends ScoringHand with Points2 {
  protected def isCompletedBy(hand: Hand) = hand.chows.size == 4
}

class AllPung extends ScoringHand with Points6 {
  protected def isCompletedBy(hand: Hand) = hand.pongLikes.size == 4
}

class AllEvenPung extends ScoringHand with Points24 {
  protected def isCompletedBy(hand: Hand) = hand.combinations.map(_.value).forall { _ match {
    case None => false
    case Some(v) => v % 2 == 0
  } }
}

class AllType extends ScoringHand with Points6 {
  protected def isCompletedBy(hand: Hand) = hand.combinations.flatMap(_.suit).distinct.size == 3 &&
    hand.tiles.count(_.isWind) >= 1 && hand.tiles.count(_.isDragon) >= 1
}

class AllSimples extends ScoringHand with Points2 {
  protected def isCompletedBy(hand: Hand) = hand.tiles.flatMap(_.value).filter(v => v > 1 && v < 9) == 14
}

class AllFive extends ScoringHand with Points16 {
  protected def isCompletedBy(hand: Hand) = hand.combinations.forall { c =>
    c.isChow && c.value == Some(4) || c.value == Some(5) && (c.isPair || c.isPongLike)
  }
}

class NoHonors extends ScoringHand with Points1 {
  protected def isCompletedBy(hand: Hand) = hand.tiles.forall(t => !(t.isDragon || t.isWind))
}

class AllGreen extends ScoringHand with Points88 {
  import com.mahjong.Tile.Suit.Bamboo
  import com.mahjong.Tile.Color.Green

  private[this] val authorizedTiles = Seq(
    Suited(Bamboo, 2),
    Suited(Bamboo, 3),
    Suited(Bamboo, 4),
    Suited(Bamboo, 6),
    Suited(Bamboo, 8),
    Dragon(Green)
  )

  protected def isCompletedBy(hand: Hand) = hand.tiles.forall(authorizedTiles contains)
}


