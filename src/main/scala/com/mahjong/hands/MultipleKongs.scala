package com.mahjong.hands

import com.mahjong.{Hand, ScoringHand}
import com.mahjong.points.{Points88, Points32, Points4, Points1}

abstract class MultipleKongs(val count: Int) extends ScoringHand {
  protected def isCompletedBy(hand: Hand) = hand.combinations.count(_.isKong) == count
}

class MeldedKong  extends MultipleKongs(1)  with Points1
class TwoKongs    extends MultipleKongs(2)  with Points4
class ThreeKongs  extends MultipleKongs(3)  with Points32
class FourKongs   extends MultipleKongs(4)  with Points88
