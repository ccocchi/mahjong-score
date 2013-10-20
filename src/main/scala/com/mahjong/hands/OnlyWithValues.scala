package com.mahjong.hands

import com.mahjong.{ScoringHand, Hand}
import com.mahjong.points.{Points12, Points24}

/**
 * Hand made up only of tiles with certain values
 *
 * @param values values that must contained tiles value
 */
abstract class OnlyWithValues(values: Seq[Int]) extends ScoringHand {
  override def isCompletedBy(hand: Hand) = hand.tiles.flatMap(_.value).forall(values contains)
}

class LowerTiles  extends OnlyWithValues(Seq(1, 2, 3)) with Points24
class MiddleTiles extends OnlyWithValues(Seq(4, 5, 6)) with Points24
class UpperTiles  extends OnlyWithValues(Seq(7, 8, 9)) with Points24

class LowerFour   extends OnlyWithValues(Seq(1, 2, 3, 4)) with Points12
class UpperFour   extends OnlyWithValues(Seq(6, 7, 8, 9)) with Points12
