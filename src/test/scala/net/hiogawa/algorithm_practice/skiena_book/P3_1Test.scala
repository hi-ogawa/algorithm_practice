package net.hiogawa.algorithm_practice.skiena_book

import org.specs2.mutable._

import scalaz._

class P3_1Test extends Specification {

  "solve_with_monads" should {

    val ex0 = "((())())()"
    ex0 in {
      P3_1.solve_with_monads(ex0) must be_==(\/-(()))
    }

    val ex1 = "(()(("
    ex1 in {
      P3_1.solve_with_monads(ex1) must be_==(-\/(5))
    }

    val ex2 = "((())()))()"
    ex2 in {
      P3_1.solve_with_monads(ex2) must be_==(-\/(8))
    }
  }

  "solve_by_mutable_array" should {

    val ex0 = "((())())()"
    ex0 in {
      P3_1.solve_by_mutable_array(ex0) must be_==(None)
    }

    val ex1 = "(()(("
    ex1 in {
      P3_1.solve_by_mutable_array(ex1).get must be_==(5)
    }

    val ex2 = "((())()))()"
    ex2 in {
      P3_1.solve_by_mutable_array(ex2).get must be_==(8)
    }
  }
}
