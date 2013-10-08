package com.mahjong.hands

import com.mahjong.{Dragon, Hand, ScoringHand}
import com.mahjong.points.{Points6, Points2, Points64, Points88}

abstract class DragonsPung(val count: Int, withPair: Boolean = false) extends ScoringHand {
  protected def isCompletedBy(hand: Hand) = {
    val dragons = hand.tiles.filter(_.isDragon)
    if (dragons.size >= 3 * count) {
      val tiles = dragons.asInstanceOf[Seq[Dragon]]
      val colorMap = tiles.groupBy(_.color)

      colorMap.count { case(_, dgs) => dgs.size >= 3 } == count &&
        (!withPair || colorMap.exists { case(_, dgs) => dgs.size == 2 })
    } else
      false
  }
}

class DragonPung extends DragonsPung(1) with Points2
class TwoDragonPungs extends DragonsPung(2) with Points6
class LittleThreeDragons extends DragonsPung(2, withPair = true) with Points64
class BigThreeDragons extends DragonsPung(3) with Points88
