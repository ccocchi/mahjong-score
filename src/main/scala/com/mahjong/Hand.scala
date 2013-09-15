package com.mahjong

class Hand(val combinations: Seq[Combination]) {
  val tiles: Seq[Tile] = combinations.flatMap(_.tiles)

  def isValid = combinations.map(_.tilesNumber).sum == 14

  def score: Int = {
    val scoringHands = ScoringHand.allHands.filter(_.isCompletedBy(this))

    val alreadyIncludedInWinningHand = scoringHands.map(_.includedHands).flatMap {
      case None => Seq()
      case Some(sc) => sc
    }.distinct

    scoringHands.diff(alreadyIncludedInWinningHand).map(_.points).sum
  }
}
