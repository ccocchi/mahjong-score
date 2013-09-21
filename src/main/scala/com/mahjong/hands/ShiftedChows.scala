package com.mahjong.hands

import com.mahjong.{Chow, Hand, FourAndAPair}
import scala.annotation.tailrec
import com.mahjong.points.{Points6, Points32, Points16}
import com.mahjong.Tile.Suit

abstract class ShiftedChows(val chowsNumber: Int, val pure: Boolean) extends FourAndAPair {

  protected def isCompletedBy(hand: Hand): Boolean = {
    if (hand.chows.size >= chowsNumber) {
      findShiftedChows(hand.chows) match {
        case None => false
        case Some(chows) =>
          chows.size == chowsNumber &&
            (!pure && Suit.values.diff(chows.flatMap(_.suit).toSet).isEmpty ||
              pure && chows.tail.forall(_.suit == chows.head.suit))
      }
    } else
      false
  }

  private def findShiftedChows(combinations: Seq[Chow]): Option[Seq[Chow]] = {

    def innerS(seq: Seq[Chow]) = inner(seq.head, seq.tail, Seq.empty)

    def maybeResult[T](seq: Seq[T]) = if (seq.isEmpty) None else Some(seq)

    @tailrec
    def inner(current: Chow, nextValues: Seq[Chow], result: Seq[Chow]): Option[Seq[Chow]] = nextValues match {
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
      case None => innerS(sortedCombinations.tail)
      case s => s
     }
  }
}

class MixedShiftedChows extends ShiftedChows(3, pure = false) with Points6
class PureShiftedChows  extends ShiftedChows(3, pure = true)  with Points16
class FourShiftedChows  extends ShiftedChows(4, pure = true)  with Points32