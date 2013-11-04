package com.mahjong

import Tile.Suit._

object Mahjong {
  def main(args: Array[String]) = {
    args.headOption match {
      case None => throw new Exception("Need a hand")
      case Some(str) =>
        val hand = new Hand(handToCombinations(str))

        println("Total = " + hand.score.toString)
    }
  }

  def handToCombinations(str: String): Seq[Combination] = {
    str.split(",").toList.map { s =>
      val tileType = s(0)
      val number = s(1)
      val suit = s(2)
      val hidden = s(3) match {
        case '+' => false
        case _ => true
      }

      lazy val t = tile(number, suit)

      tileType match {
        case 'p' => Pong(t, hidden)
        case 'c' => Chow(t, hidden)
        case 'k' => Kong(t, hidden)
        case '2' => Pair(t, hidden)
        case _ => throw new Exception("unknown type")
      }
    }
  }


  def tile(number: Char, suit: Char): Tile = number match {
    case 'd' => Dragon(suit)
    case 'v' => Wind(suit)
    case _ =>
      val s = suit match {
        case 'c' => Character
        case 'b' => Bamboo
        case 'd' => Dot
      }
      Suited(s, number.toString.toInt)
  }

}
