package com.mahjong

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

class ScoringHandSpec extends FunSpec with ShouldMatchers {
  import Tile.Suit._
  import Tile.Color._

  it("should refuse hand with not enough tiles")

  it("should reject invalid hands")

  it("should found the right score") {
    val combinations = Seq(
      Chow(1, Bamboo, hidden = true),
      Chow(1, Character, hidden = true),
      Chow(4, Dot, hidden = true),
      Chow(1, Dot),
      new Pair(Seq(
        Suited(Bamboo, 4),
        Suited(Bamboo, 4)
      ))
    )

    val hand = new Hand(combinations)
    hand.score should equal(11)
  }

  it("should do something") {
    val combinations = Seq(
      Chow(1, Bamboo, hidden = true),
      Chow(4, Bamboo, hidden = true),
      Chow(7, Bamboo, hidden = true),
      Chow(1, Dot, hidden = true),
      new Pair(Seq(
        Suited(Bamboo, 9),
        Suited(Bamboo, 9)
      )) {
        override protected val concealed = true
      }
    )

    val hand = new Hand(combinations)
    hand.score should equal(22)
  }

  it("should not fail") {
    val combinations = Seq(
      Chow(5, Bamboo, hidden = true),
      Chow(5, Character, hidden = true),
      Chow(4, Dot, hidden = true),
      Chow(5, Dot, hidden = true),
      new Pair(Seq(
        Suited(Dot, 6),
        Suited(Dot, 6)
      ))
    )

    val hand = new Hand(combinations)
    hand.score should equal(16) // should be 18 with AllConcealedAndDraw
  }

  it("should do oher things") {
    val combinations = Seq(
      Chow(7, Bamboo),
      Chow(4, Character),
      PongFactory.ofWind(Wind.Direction.South),
      Chow(1, Dot, hidden = true),
      PairFactory.ofSuited(1, Dot)
    )

    val hand = new Hand(combinations)
    hand.score should equal(9) // should be 10 with dominant wind
  }
}
