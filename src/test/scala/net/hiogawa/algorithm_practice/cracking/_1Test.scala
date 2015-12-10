package net.hiogawa.algorithm_practice.cracking

import org.specs2.mutable._

class _1Test extends Specification {

  val ex0 = "aabcccccaaa"

  ex0 >> {
    _1._6(ex0) must be_==("a2b1c5a3")
  }
}
