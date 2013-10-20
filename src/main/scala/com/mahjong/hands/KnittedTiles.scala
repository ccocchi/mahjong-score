package com.mahjong.hands

import com.mahjong.{Hand, ScoringHand}
import scala.annotation.tailrec
import com.mahjong.points.{Points24, Points12}

abstract class KnittedTiles(complete: Boolean, honorCount: Option[Int]) extends ScoringHand {
  protected def isCompletedBy(hand: Hand) = {
    val knittedMap = hand.tiles.groupBy(_.suit).filter(_._1.isDefined).mapValues(_.flatMap(_.value).sorted)
                         .filter { case (_, vs) => isKnitted(vs) }

    val completeCondition = complete && knittedMap.count { case(_, vs) => vs.size == 3 } == 3 ||
      !complete && knittedMap.size == 3 && knittedMap.values.map(_.size).sum == (14 - honorCount.get)

    val honorCondition = honorCount match {
      case None => true
      case Some(count) => hand.tiles.count(t => t.isDragon || t.isWind) == count
    }

    completeCondition && honorCondition
  }

  private def isKnitted(seq: Seq[Int]): Boolean = {
    @tailrec
    def inner(current: Int, rest: List[Int]): Boolean = rest match {
      case head :: tail =>
        if (head == current + 3 || head == current + 6)
          inner(head, tail)
        else
          false
      case Nil => true
    }

    inner(seq.head, seq.tail.toList)
  }
}

class KnittedStraight extends KnittedTiles(complete = true, None)   with Points12
class BigSnaky extends KnittedTiles(complete = false, Some(7))      with Points24
class LittleSnaky1 extends KnittedTiles(complete = true, Some(5))   with Points12
class LittleSnaky2 extends KnittedTiles(complete = false, Some(6))  with Points12

