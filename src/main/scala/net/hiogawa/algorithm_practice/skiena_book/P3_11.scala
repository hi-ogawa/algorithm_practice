package net.hiogawa.algorithm_practice.skiena_book

object P3_11 {

  object Cll {
    def init(elems: TraversableOnce[Int]) : Cll = {
      Cll(elems.toArray)
    }
  }

  case class Cll(arr: Array[Int]) {
    def smallest : Int => Int => Int =
      i => j => {
        if (i == j) {
          arr(i)
        } else {
          val m = (i + j) / 2
          val former = smallest(i)(m)
          val latter = smallest(m + 1)(j)
          if (former < latter) former else latter
        }
      }
  }
}
