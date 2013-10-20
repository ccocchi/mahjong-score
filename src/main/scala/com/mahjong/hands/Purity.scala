package com.mahjong.hands

import com.mahjong.{Hand, ScoringHand}
import com.mahjong.points.{Points24, Points6}

abstract class Purity(withHonor: Boolean) extends ScoringHand {
  protected def isCompletedBy(hand: Hand) = {
    val suitsMap = hand.tiles.groupBy(_.suit)
    !withHonor && suitsMap.size == 1 || withHonor && suitsMap.size == 2 && suitsMap.contains(None)
  }
}

class HalfPureHand  extends Purity(withHonor = true) with Points6
class PureHand      extends Purity(withHonor = false) with Points24
