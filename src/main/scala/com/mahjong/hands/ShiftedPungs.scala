package com.mahjong.hands

import com.mahjong.{PongLike, Hand, ScoringHand}
import scala.annotation.tailrec
import com.mahjong.Tile.Suit
import com.mahjong.points.{Points48, Points24, Points8}


abstract class ShiftedPungs(val pungsNumber: Int, val pure: Boolean) extends ScoringHand {

  protected def isCompletedBy(hand: Hand): Boolean = {
    if (hand.pongLikes.size >= pungsNumber) {
      findShiftedPongLikes(hand.pongLikes) match {
        case None => false
        case Some(pungs) =>
          pungs.size == pungsNumber &&
            (!pure && Suit.values.diff(pungs.flatMap(_.suit).toSet).isEmpty ||
              pure && pungs.tail.forall(_.suit == pungs.head.suit))
      }
    } else
      false
  }

  private def findShiftedPongLikes(combinations: Seq[PongLike]): Option[Seq[PongLike]] = {

    def innerS(seq: Seq[PongLike]) = inner(seq.head, seq.tail.toList, Seq.empty)

    def maybeResult[T](seq: Seq[T]) = if (seq.isEmpty) None else Some(seq)

    @tailrec
    def inner(current: PongLike, nextValues: Seq[PongLike], result: Seq[PongLike]): Option[Seq[PongLike]] =
      nextValues match {
        case head :: tail =>
          val newResult = result :+ current
          if (current.getValue + 1 == head.getValue)
            inner(head, tail, newResult)
          else
            maybeResult(newResult)
        case Nil => maybeResult(result)
      }

    val sortedCombinations = combinations.sortBy(_.getValue)
    innerS(sortedCombinations) match {
      case None => innerS(sortedCombinations.tail.toList)
      case s => s
    }
  }
}

class ThreeShiftedPungs     extends ShiftedPungs(3, pure = false) with Points8
class ThreePureShiftedPungs extends ShiftedPungs(3, pure = true)  with Points24
class FourPureShiftedPungs  extends ShiftedPungs(4, pure = true)  with Points48
