package com.mahjong.hands

import com.mahjong.{FourAndAPair, Hand, ScoringHand}
import com.mahjong.points.{Points64, Points16, Points2}

abstract class ConcealedPung(val count: Int) extends ScoringHand {
  protected def isCompletedBy(hand: Hand) = hand.combinations.count(c => c.isPong && c.isHidden) == count
}

class TwoConcealedPungs   extends ConcealedPung(2) with Points2
class ThreeConcealedPungs extends ConcealedPung(3) with Points16
class FourConcealedPungs  extends ConcealedPung(4) with FourAndAPair with Points64
