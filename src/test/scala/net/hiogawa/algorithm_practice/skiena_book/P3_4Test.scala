package net.hiogawa.algorithm_practice.skiena_book

import org.specs2.mutable._
import P3_4.IntDict

class P3_4Test extends Specification {

  val dict = IntDict.init(5, List(0, 3, 4, 4, 0, 2))

  "insert - delete - search" >> {

    dict.search(1) must be_==(false)
    dict.search(2) must be_==(true)
    dict.delete(2)
    dict.search(2) must be_==(false)
    dict.insert(2)
    dict.search(2) must be_==(true)
  }
}
