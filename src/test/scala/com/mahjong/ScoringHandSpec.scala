package com.mahjong

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

class ScoringHandSpec extends FunSpec with ShouldMatchers {
  import Tile.Suit._

  describe("FourAndAPair") {
    val scoringHand = new FourAndAPair {
      val points = 0
    }

    it("should be composed of 4 three cards combination and a pair") {
      val combinations = 1 to 4 map(i => PongFactory.ofSuited(i))
      val hand = new Hand(combinations :+ PairFactory.ofSuited(5))

      hand should be('valid)
      scoringHand.isCompletedBy(hand) should be(true)
    }

    it("should accept kongs") {
      val combinations = 1 to 3 map(i => PongFactory.ofSuited(i))
      val hand = new Hand(combinations :+ PongFactory.ofSuited(4, isKong = true) :+ PairFactory.ofSuited(5))

      hand should be('valid)
      scoringHand.isCompletedBy(hand) should be(true)
    }

    it("should not composed special hands") {
      val combinations = 1 to 7 map(i => PairFactory.ofSuited(i))
      val hand = new Hand(combinations)

      hand should be('valid)
      scoringHand.isCompletedBy(hand) should be(false)
    }
  }

  describe("FourKongs") {
    val scoringHand = new FourKongs

    it("should be completed by a hand") {
      val combinations = 1 to 4 map(i => PongFactory.ofSuited(i, isKong = true))
      val hand = new Hand(combinations)
      scoringHand.isCompletedBy(hand)
    }
  }

  describe("NineGates") {
    val scoringHand = new NineGates

    it("should be completed by a hand") {
      val pongs = Seq(1, 9) map(i => PongFactory.ofSuited(i))
      val pair = PairFactory.ofSuited(2)
      val otherTiles = 3 to 8 map(i => new SingleTile(Suited(Bamboo, i)))
      val hand = new Hand(pongs ++ otherTiles :+ pair)

      hand should be('valid)
      scoringHand.isCompletedBy(hand) should be(true)
    }
  }
}
