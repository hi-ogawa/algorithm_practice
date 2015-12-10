package net.hiogawa.algorithm_practice.skiena_book

object P3_4 {

  object IntDict {
    def init(max : Int, elems: TraversableOnce[Int]) : IntDict = {
      var arr = scala.collection.mutable.ArraySeq.range(1, max + 1).map(_ => 0)
      elems.foreach( x =>
        arr(x) = arr(x) + 1
      )
      IntDict(arr)
    }
  }

  case class IntDict(arr: scala.collection.mutable.ArraySeq[Int]) {

    def insert : Int => Unit =
      e => arr(e) = arr(e) + 1

    def delete : Int => Unit =
      e => if (arr(e) > 0) arr(e) = arr(e) - 1

    def search : Int => Boolean =
      e => arr(e) > 0
  }
}
