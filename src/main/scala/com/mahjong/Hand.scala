package com.mahjong

class Hand(val combinations: Seq[Combination]) {
  val tiles: Seq[Tile] = combinations.flatMap(_.tiles)

  lazy val chows = combinations.filter(_.isChow).asInstanceOf[Seq[Chow]]
  lazy val chowValues = chows.flatMap(_.value).sorted

  lazy val pongLikes = combinations.filter(_.isPongLike).asInstanceOf[Seq[PongLike]]
  lazy val pongs = combinations.filter(_.isPong).asInstanceOf[Seq[Pong]]

  def isValid = combinations.map(_.tilesNumber).sum == 14

  def score: Int = {
    val scoringHands = ScoringHand.allHands.map { h => (h, h.getPoints(this)) }.filter(_._2 > 0)

    val alreadyIncludedInWinningHand = scoringHands.flatMap(_._1.includedHands).distinct

    val sc = scoringHands.filterNot { case (h, _) => alreadyIncludedInWinningHand.contains(h.name) }

    println(sc.map(_._1.name))

    sc.map(_._2).sum
  }
}
