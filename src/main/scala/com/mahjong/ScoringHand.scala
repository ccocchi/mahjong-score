package com.mahjong

import scala.annotation.tailrec
import Tile.Suit._
import Tile.Color._
import com.mahjong.hands._

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
    // 88
    new BigFourWinds,
    new BigThreeDragons,
    new AllGreen,
    new NineGates,
    new FourKongs,
    new SevenShiftedPair,
    // new ThirteenOrphans

    // 64
    new AllTerminals,
    new LittleFourWinds,
    new LittleThreeDragons,
    new AllHonor,
    new FourConcealedPungs,
    new TwoDragonsInAFamily,

    // 48
    new QuadrupleChows,
    new FourPureShiftedPungs,

    // 32
    new FourPureShiftedChows,
    new ThreeKongs,
    new AllTerminalAndHonor,

    // 24
    // new SevenPairs,
    // new KnittedTiles,
    new AllEvenPung,
    new PureHand,
    new TriplePureChows,
    new ThreePureShiftedPungs,
    new LowerTiles,
    new MiddleTiles,
    new UpperTiles,

    // 16
    new PureBigStraight,
    new TwoDragonsInThreeFamilies,
    new ThreePureShiftedChows,
    new AllFive,
    new TriplePung,
    new ThreeConcealedPungs,

    // 12
    // new SmallKnittedTiles,
    // new StraightKnittedTiles,
    new LowerFour,
    new UpperFour,
    new BigThreeWinds,

    // 8
    new BigStraight,
    new Symmetry,
    new TripleChows,
    new ThreeShiftedPungs,
    // new ValueLessHand,

    // 6
    new AllPung,
    new HalfPureHand,
    new ThreeShiftedChows,
    new AllType,
    new AllRevealed,
    new TwoDragonPungs,

    // 4
    // new HonorOrTerminalEverywhere,
    new TwoKongs,

    // 2
    new DragonPung,
    new AllConcealed,
    new AllChow,
    // new FourOfTheSame,
    new DoublePung,
    new TwoConcealedPungs,
    // new ConcealedKong,
    new AllSimples,

    // 1
    new PureDoubleChow,
    new MixedDoubleChow,
    new PureLittleStraight,
    new OneDragonInAFamily,
    // new TerminalOrHonorPung,
    // new ExposedKong,
    // new MissingFamily,
    // new NoHonor

  )
}

abstract class ScoringHand {
  val points: Int
  val includedHands: Option[Seq[ScoringHand]] = None

  def isScoring(hand: Hand): Boolean = baseVerify(hand) && isCompletedBy(hand)

  protected def isCompletedBy(hand: Hand): Boolean
  protected def baseVerify(hand: Hand): Boolean = true
}

trait GlobalCondition extends ScoringHand

trait FourAndAPair extends ScoringHand {
  override def baseVerify(hand: Hand) = hand.combinations.size == 5 && hand.combinations.count(_.isPair) == 1
}

trait SpecialHand extends ScoringHand

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

class NineGates extends SpecialHand {
  val points = 88

  def isCompletedBy(hand: Hand) = {
    val pongs = hand.combinations.filter(_.isPong)
    val pongsValues = pongs.flatMap(_.value).sorted
    val remainingTiles = hand.tiles.diff(pongs.flatMap(_.tiles)).flatMap(_.value).distinct.sorted

    pongs.size == 2 && pongsValues == Seq(1, 9) && remainingTiles.containsSlice(2 to 8) && hand.isFlush
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


class FourHiddenPungs extends FourAndAPair {
  val points = 64

  protected def isCompletedBy(hand: Hand) = hand.combinations.count(c => c.isPong && c.isHidden) == 4
}








