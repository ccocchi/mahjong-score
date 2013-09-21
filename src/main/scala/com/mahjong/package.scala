package com

package object mahjong {
  implicit def toRichCombinationSeq(cs: Seq[Combination]) = new RichTypedSeq(cs)
  //implicit def toRichTileSeq(ts: Seq[Tile]) = new RichTypedSeq[Tile](ts)

  class RichTypedSeq(cs: Seq[Combination]) {
    def tcount[V](): Int = {
      cs.count {
        case c: V => true
        case c => false
      }
    }

    def tfilter[V <: Combination](): Seq[Combination] = cs.filter {
      case c: V => true
      case _ => false
    }
  }
}
