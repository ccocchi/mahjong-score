package com.mahjong

import com.mahjong.hands._

object ScoringHand {
  def allHands: Seq[ScoringHand] = Seq(
    // 88
    new BigFourWinds,
    new BigThreeDragons,
    new AllGreen,
    // new NineGates,
    new FourKongs,
    // new SevenShiftedPair,
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
    new BigSnaky,
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
    new KnittedStraight,
    new LittleSnaky1,
    new LittleSnaky2,
    new LowerFour,
    new UpperFour,
    new BigThreeWinds,

    // 8
    new BigStraight,
    new Symmetry,
    new TripleChows,
    new ThreeShiftedPungs,
    // new ValueLessHand,
    new TwoConcealedKongs,

    // 6
    new AllPung,
    new HalfPureHand,
    new ThreeShiftedChows,
    new AllType,
    new AllRevealed,
    new TwoDragonPungs,

    // 4
    new HonorOrTerminalEverywhere,
    new TwoKongs,

    // 2
    new DragonPung,
    new AllConcealed,
    new AllChow,
    new FourOfTheSame,
    new DoublePung,
    new TwoConcealedPungs,
    new OneConcealedKong,
    new AllSimples,

    // 1
    new PureDoubleChow,
    new MixedDoubleChow,
    new PureLittleStraight,
    new OneDragonInAFamily,
    new TerminalOrWindPung,
    new MeldedKong,
    new OneMissingFamily,
    new NoHonors

  )
}

abstract class ScoringHand {
  val name: String = getClass.toString
  val points: Int
  val includedHands: Option[Seq[String]] = None

  def getPoints(hand: Hand): Int = if (isCompletedBy(hand)) points else 0
  protected def isCompletedBy(hand: Hand): Boolean
}









