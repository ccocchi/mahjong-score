package com.mahjong

class Hand(val combinations: Seq[Combination]) {
  val tiles: Seq[Tile] = combinations.flatMap(_.tiles)

  lazy val chows = combinations.filter(_.isChow).asInstanceOf[Seq[Chow]]
  lazy val chowValues = chows.flatMap(_.value).sorted

  lazy val pongLikes = combinations.filter(_.isPongLike).asInstanceOf[Seq[PongLike]]

  lazy val pongs = combinations.filter(_.isPong).asInstanceOf[Seq[Pong]]

  def isValid = combinations.map(_.tilesNumber).sum == 14

  def score: Int = {
    val scoringHands = ScoringHand.allHands.filter(_.isScoring(this))

    val alreadyIncludedInWinningHand = scoringHands.map(_.includedHands).flatMap {
      case None => Seq()
      case Some(sc) => sc
    }.distinct

    scoringHands.diff(alreadyIncludedInWinningHand).map(_.points).sum
  }
}
