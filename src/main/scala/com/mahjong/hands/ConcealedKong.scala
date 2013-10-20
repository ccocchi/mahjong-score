package com.mahjong.hands

import com.mahjong.{Hand, ScoringHand}
import com.mahjong.points.{Points8, Points2}

abstract class ConcealedKong(val count: Int) extends ScoringHand {
  protected def isCompletedBy(hand: Hand) = hand.combinations.count(c => c.isKong && c.isHidden) == count
}

class OneConcealedKong  extends ConcealedKong(1) with Points2
class TwoConcealedKongs extends ConcealedKong(2) with Points8
