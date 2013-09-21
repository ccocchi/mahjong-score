package com.mahjong

import scala.annotation.tailrec
import Tile.Suit._
import Tile.Color._
import Wind.Direction._

object Helpers {
  def isSequential[T <: Suited](tiles: Seq[T], interval: Int = 1): Boolean = isSequential(tiles.flatMap(_.value).sorted)

  def isSequential[I <: Int](values: Seq[I], interval: Int = 1): Boolean = {
    require(interval == 1 || interval == 2)

    @tailrec
    def inner(ts: Seq[Int], previousValue: Int): Boolean = ts match {
      case head :: tail =>
        if (head == previousValue + interval)
          inner(tail, previousValue + interval)
        else
          false
      case Nil => true
    }

    if (values.isEmpty)
      false
    else {
      inner(values.tail, values.head)
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

  def isScoring(hand: Hand): Boolean = baseVerify(hand) && isCompletedBy(hand)

  protected def isCompletedBy(hand: Hand): Boolean
  protected def baseVerify(hand: Hand): Boolean = true
}

abstract class FourAndAPair extends ScoringHand {
  override def baseVerify(hand: Hand) = hand.combinations.size == 5 && hand.combinations.count(_.isPair) == 1
}

class FourKongs extends FourAndAPair {
  val points = 88
  protected def isCompletedBy(hand: Hand) = hand.combinations.count(_.isKong) == 4
}

class ThirteenOrphans extends ScoringHand {
  val points = 88
  def isCompletedBy(hand: Hand) = hand.combinations.count(_.isInstanceOf[SingleTile]) == 12 &&
    hand.tiles.count(t => !t.terminal && !t.honor) == 0
}

class SevenShiftedPair extends ScoringHand {
  val points = 88

  import Helpers._

  def isCompletedBy(hand: Hand): Boolean = {
    if (hand.combinations.count(_.isPair) == 7 && hand.tiles.count(_.isInstanceOf[Suited]) == 14) {
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
    val pongs = hand.combinations.filter(_.isPong)
    val pongsValues = pongs.flatMap(_.value).sorted
    val remainingTiles = hand.tiles.diff(pongs.flatMap(_.tiles)).flatMap(_.value).distinct.sorted

    pongs.size == 2 && pongsValues == Seq(1, 9) && isFlush(hand) && remainingTiles.containsSlice(2 to 8)
  }
}

class AllGreen extends FourAndAPair {
  val points = 88

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

class BigThreeDragons extends FourAndAPair {
  val points = 88

  private[this] val threeColors = Seq(Green, Red, White)

  protected def isCompletedBy(hand: Hand) = threeColors.forall { c =>
    val dragon = Dragon(c)
    hand.tiles.count(_ == dragon) >= 3
  }
}

class BigFoudWinds extends FourAndAPair {
  val points = 88

  private[this] val fourDirections = Seq(North, South, East, West)

  protected def isCompletedBy(hand: Hand) = fourDirections.forall { d =>
    val wind = Wind(d)
    hand.tiles.count(_ == wind) >= 3
  }
}

class FourHiddenPungs extends FourAndAPair {
  val points = 64

  protected def isCompletedBy(hand: Hand) = hand.combinations.count(c => c.isPong && c.isHidden) == 4
}

class LittleThreeDragons extends FourAndAPair {
  val points = 64

  protected def isCompletedBy(hand: Hand): Boolean = {
    val dragons = hand.tiles.filter(_.isDragon)
    if (dragons.size == 7) {
      val tiles = dragons.asInstanceOf[Seq[Dragon]]
      val dragonMap = tiles.groupBy(d => d.color).mapValues(_.size)

      dragonMap.size == 3 && dragonMap.values.toList.sorted == Seq(1, 3, 3)
    } else
      false
  }
}

class LittleFourWinds extends FourAndAPair {
  val points = 64

  protected def isCompletedBy(hand: Hand): Boolean = {
    val winds = hand.tiles.filter(_.isWind)
    if (winds.size == 10) {
      val tiles = winds.asInstanceOf[Seq[Wind]]
      val windMap = tiles.groupBy(w => w.direction).mapValues(_.size)

      windMap.size == 4 && windMap.values.toList.sorted == Seq(1, 3, 3, 3)
    } else
      false
  }
}

class AllTerminals extends FourAndAPair {
  val points = 64

  protected def isCompletedBy(hand: Hand) = hand.tiles.forall(_.terminal)
}

class FourPureShiftedPungs extends FourAndAPair {
  val points = 48

  import Helpers._

  protected def isCompletedBy(hand: Hand) = {
    val pongs = hand.combinations.filter(_.isPongLike).asInstanceOf[Seq[PongLike]]
    pongs.size == 4 && isFlush(hand) && isSequential(pongs.flatMap(_.value).sorted)
  }
}

class QuadrupleChow extends FourAndAPair {
  val points = 48

  import Helpers._

  protected def isCompletedBy(hand: Hand) = {
    val chows = hand.combinations.filter(_.isChow).asInstanceOf[Seq[Chow]]
    chows.size == 4 && isFlush(hand) && chows.tail.foldLeft((chows.head, true)) { case(tuple, c) =>
      (c, tuple._2 && tuple._1 == c)
    }._2
  }
}

class AllTerminalAndHonor extends FourAndAPair {
  val points = 32

  protected def isCompletedBy(hand: Hand) = hand.tiles.forall(t => t.terminal || t.isDragon || t.isWind)
}

class ThreeKongs extends FourAndAPair {
  val points = 32

  protected def isCompletedBy(hand: Hand) = hand.combinations.count(_.isKong) == 4
}

class FourShiftedChows extends FourAndAPair {
  val points = 32

  import Helpers._

  protected def isCompletedBy(hand: Hand) = {
    val chows = hand.combinations.filter(_.isChow).asInstanceOf[Seq[Chow]]
    val values = chows.flatMap(_.value).sorted

    chows.size == 4 && isFlush(hand) && (isSequential(values) || isSequential(values, interval = 2))
  }
}






