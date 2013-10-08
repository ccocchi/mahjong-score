package com.mahjong.hands

import com.mahjong.{Wind, ScoringHand, Hand}
import com.mahjong.points.{Points88, Points64, Points16}

abstract class WindPungs(val count: Int, withPair: Boolean = false) extends ScoringHand {
  protected def isCompletedBy(hand: Hand) = {
    val winds = hand.tiles.filter(_.isWind)
    if (winds.size >= 3 * count) {
      val tiles = winds.asInstanceOf[Seq[Wind]]
      val directionMap = tiles.groupBy(_.direction)

      directionMap.count { case(_, wds) => wds.size >= 3 } == count &&
        (!withPair || directionMap.exists { case(_, wds) => wds.size == 2 })
    } else
      false
  }
}

class BigThreeWinds   extends WindPungs(3) with Points16
class LittleFourWinds extends WindPungs(3, withPair = true) with Points64
class BigFourWinds    extends WindPungs(4) with Points88
