package net.hiogawa.algorithm_practice.skiena_book

import org.scalacheck.Properties
import org.scalacheck.Prop.{forAll, BooleanOperators}

class P3_11Test extends Properties("Cll") {

  property("Cll.smallest") = forAll { (l : Array[Int], i : Int, j : Int) =>
    (0 <= i && i <= j && j <= l.length - 1) ==> {
      P3_11.Cll.init(l).smallest(i)(j) == l.slice(i, j + 1).min
    }
  }
}
