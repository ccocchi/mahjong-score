package com.mahjong.hands

import com.mahjong.{Chow, Hand, ScoringHand}
import com.mahjong.points.{Points16, Points8, Points1}
import scala.annotation.tailrec

abstract class Straight(val count: Int, pure: Boolean) extends ScoringHand {
  protected def isCompletedBy(hand: Hand) = {
    if (hand.chows.size >= count)
      count match {
        case 2 => findStraight(hand.chows).size == count || findStraight(hand.chows.drop(2)).size == count
        case _ => findStraight(hand.chows).size == count
      }
    else
      false
  }

  protected def findStraight(chows: Seq[Chow]): Seq[Chow] = {
    @tailrec
    def inner(list: Seq[Chow], acc: Seq[Chow]): Seq[Chow] = {
      val last = acc.last

      list match {
        case head :: tail if head.value != last.value && (head.value.get + last.value.get) % 3 == 2 =>
          if (pure && head.suit == last.suit || !pure && !acc.map(_.suit).contains(head.suit))
            inner(tail, acc :+ head)
          else
            inner(tail, acc)
        case head :: tail => inner(tail, acc)
        case Nil => acc
      }
    }

    val list = chows.tail

    val result = inner(list, Seq(chows.head))
    result.size match {
      case 1 => inner(list.tail, Seq(list.head))
      case _ => result
    }
  }
}

class PureLittleStraight  extends Straight(2, pure = true)  with Points1
class BigStraight         extends Straight(3, pure = false) with Points8
class PureBigStraight     extends Straight(3, pure = true)  with Points16 {
  override val includedHands = Seq("OneDragonInAFamily")
}