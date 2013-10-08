package com.mahjong.hands

import com.mahjong.{Hand, ScoringHand}
import com.mahjong.points.{Points8, Points24, Points48, Points1}

abstract class MultipleChows(val count: Int, val pure: Boolean) extends ScoringHand {
  protected def isCompletedBy(hand: Hand) = {
    if (hand.chows.size >= count) {
      hand.chows.groupBy(_.getValue).exists { case(_, chows) =>
        val suits = chows.flatMap(_.suit).distinct

        chows.size == count && (pure && suits.size == count || !pure && suits.size == count)
      }
    } else
      false
  }
}

class MixedDoubleChow extends MultipleChows(2, pure = false)  with Points1
class PureDoubleChow  extends MultipleChows(2, pure = true)   with Points1
class TripleChows     extends MultipleChows(3, pure = false)  with Points8
class TriplePureChows extends MultipleChows(3, pure = true)   with Points24
class QuadrupleChows  extends MultipleChows(4, pure = true)   with Points48