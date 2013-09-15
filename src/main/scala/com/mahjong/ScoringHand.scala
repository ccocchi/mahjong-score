package com.mahjong

import scala.annotation.tailrec
import Tile.Suit._
import Tile.Color._

object Helpers {
  def isSequential(tiles: Seq[Suited]): Boolean = {

    @tailrec
    def inner(ts: Seq[Int], previousValue: Int): Boolean = ts match {
      case head :: tail =>
        if (head == previousValue + 1)
          inner(tail, previousValue + 1)
        else
          false
      case Nil => true
    }

    if (tiles.isEmpty)
      false
    else {
      val sortedTiles = tiles.flatMap(_.value).sorted
      inner(sortedTiles.tail, sortedTiles.head)
    }
  }

  def isFlush(tiles: Seq[Suited]): Boolean = {
    val suit = tiles.head.suit
    tiles.tail.forall(_.suit == suit)
  }

  def isFlush(hand: Hand): Boolean = {
    if (hand.tiles.count(_.isInstanceOf[Suited]) == 14) {
      val suitedTiles = hand.tiles.asInstanceOf[Seq[Suited]]
      isFlush(suitedTiles)
    } else
      false
  }
}

object ScoringHand {
  val allHands: Seq[ScoringHand] = Seq(
    new FourKongs,
    new ThirteenOrphans
  )
}

abstract class ScoringHand {
  val points: Int
  val includedHands: Option[Seq[ScoringHand]] = None

  def isCompletedBy(hand: Hand): Boolean
}

abstract class FourAndAPair extends ScoringHand {
  def isCompletedBy(hand: Hand) = hand.combinations.size == 5 && hand.combinations.count(_.isInstanceOf[Pair]) == 1
}

class FourKongs extends FourAndAPair {
  val points = 88
  override def isCompletedBy(hand: Hand) =
    super.isCompletedBy(hand) && hand.combinations.count(_.isInstanceOf[Kong]) == 4
}

class ThirteenOrphans extends ScoringHand {
  val points = 88
  def isCompletedBy(hand: Hand) =
    hand.combinations.count(_.isInstanceOf[SingleTile]) == 12 && hand.tiles.count(t => !t.terminal && !t.honor) == 0
}

class SevenShiftedPair extends ScoringHand {
  val points = 88

  import Helpers._

  def isCompletedBy(hand: Hand): Boolean = {
    if (hand.combinations.count(_.isInstanceOf[Pair]) == 7 && hand.tiles.count(_.isInstanceOf[Suited]) == 14) {
      val tiles = hand.tiles.asInstanceOf[Seq[Suited]]
      return isFlush(tiles) && isSequential(tiles)
    }
    false
  }
}

class NineGates extends ScoringHand {
  val points = 88

  import Helpers.isFlush

  def isCompletedBy(hand: Hand) = {
    val pongs = hand.combinations.filter(_.isInstanceOf[Pong])
    val pongsValues = pongs.flatMap(_.value).sorted
    val remainingTiles = hand.tiles.diff(pongs.flatMap(_.tiles)).flatMap(_.value).distinct.sorted

    pongs.size == 2 && pongsValues == Seq(1, 9) && isFlush(hand) && remainingTiles.containsSlice(2 to 8)
  }
}

class AllGreen extends FourAndAPair {
  val points = 88

  private[this] val authorizedTiles = Seq(
    new Suited(Bamboo, 2),
    new Suited(Bamboo, 3),
    new Suited(Bamboo, 4),
    new Suited(Bamboo, 6),
    new Suited(Bamboo, 8),
    new Dragon(Green)
  )

  override def isCompletedBy(hand: Hand) = super.isCompletedBy(hand) && hand.tiles.forall(authorizedTiles contains)
}

class BigThreeDragons extends FourAndAPair {
  val points = 88

  private[this] val threeColors = Seq(Green, Red, White)

  override def isCompletedBy(hand: Hand) = super.isCompletedBy(hand) && threeColors.forall { c =>
    val dragon = new Dragon(c)
    hand.tiles.count(_ == dragon) == 3
  }
}

