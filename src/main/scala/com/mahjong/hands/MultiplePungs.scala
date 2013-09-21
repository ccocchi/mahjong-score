package com.mahjong.hands

import com.mahjong.{Hand, ScoringHand}
import com.mahjong.points.{Points16, Points2}

abstract class MultiplePungs(val count: Int) extends ScoringHand {
  protected def isCompletedBy(hand: Hand) = hand.pongs.size >= count &&
    hand.pongs.groupBy(_.getValue).exists { case(_, pongs) =>
      pongs.size == count && pongs.flatMap(_.suit).distinct.size == count
    }
}

class DoublePung extends MultiplePungs(2) with Points2
class TriplePung extends MultiplePungs(3) with Points16
