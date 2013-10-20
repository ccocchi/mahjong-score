package com.mahjong.hands

import com.mahjong.{Hand, ScoringHand}
import com.mahjong.points.{Points16, Points64, Points1}

abstract class DragonFamily(count: Int, sameFamily: Boolean) extends ScoringHand {
  protected def isCompletedBy(hand: Hand) = {
    val chowMap = hand.chows.filter { c => c.getValue == 1 || c.getValue == 7 }.groupBy(_.suit)
                      .mapValues(_.map(_.getValue))

    if (count == 1) {
      chowMap.exists { case (_, cs) => cs.contains(1) && cs.contains(7) }
    } else {
      val pairOption = hand.combinations.find(_.isPair)

      pairOption match {
        case None => false
        case Some(pair) if pair.value != Some(5) => false
        case Some(pair) =>
          val dragonsPerSuit = chowMap.mapValues { cs => Seq(cs.count(_ == 1), cs.count(_ == 7)).min }
          if (sameFamily) {
            dragonsPerSuit.find(_._2 == 2) match {
              case None => false
              case Some((suit, _)) => pair.suit == suit
            }
          } else {
            val dragons = dragonsPerSuit.filter(_._2 == 1)
            dragons.size == count && !dragons.keys.toList.contains(pair.suit)
          }
      }
    }
  }
}

class OneDragonInAFamily extends DragonFamily(1, sameFamily = true) with Points1
class TwoDragonsInThreeFamilies extends DragonFamily(2, sameFamily = false) with Points16
class TwoDragonsInAFamily extends DragonFamily(2, sameFamily = true) with Points64