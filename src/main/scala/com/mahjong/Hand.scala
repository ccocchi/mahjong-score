package com.mahjong

object Hand {
  def apply(str: String): Hand = null
}

class Hand(val combinations: Seq[Combination]) {
  val tiles: Seq[Tile] = combinations.flatMap(_.tiles)

  lazy val chows = combinations.filter(_.isChow).asInstanceOf[Seq[Chow]]
  lazy val chowValues = chows.flatMap(_.value).sorted

  lazy val pongLikes = combinations.filter(_.isPongLike).asInstanceOf[Seq[PongLike]]
  lazy val pongs = combinations.filter(_.isPong).asInstanceOf[Seq[Pong]]

  def isValid = combinations.map(_.tilesNumber).sum == 14

  def winningScoringHands = {
    val scoringHands = ScoringHand.allHands.map { h => (h, h.getPoints(this)) }.filter(_._2 > 0)
    val alreadyIncludedInWinningHand = scoringHands.flatMap(_._1.includedHands).distinct

    scoringHands.filterNot { case (h, _) => alreadyIncludedInWinningHand.contains(h.name) }
  }

  def score: Int = {
    val sc = winningScoringHands

    println("Scoring hands:")
    sc.foreach(h => println("  - " + h._1.name + " (%d)".format(h._2)))

    sc.map(_._2).sum
  }
}
