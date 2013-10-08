package com.mahjong.hands

import com.mahjong.{Hand, ScoringHand}
import com.mahjong.points.{Points16, Points8, Points1}

abstract class Straight(val count: Int, pure: Boolean) extends ScoringHand {
  protected def isCompletedBy(hand: Hand) = {
    if (hand.chows.size >= count) {
      val chowsMap = hand.chows.groupBy(_.getValue)
      val baseValue = chowsMap.minBy(_._1)

      val straight = chowsMap.filter { case(v, _) => (baseValue._1 + v) % 3 == 2 }.flatMap(_._2) ++ baseValue._2
      val suits = straight.flatMap(_.suit).toSet

      straight.size == count && (!pure && suits.size == count || pure && suits.size == 1)
    } else
      false
  }
}

class PureLittleStraight  extends Straight(2, pure = true)  with Points1
class BigStraight         extends Straight(3, pure = false) with Points8
class PureBigStraight     extends Straight(3, pure = true)  with Points16