package net.hiogawa.algorithm_practice.skiena_book

import scalaz._

object P3_1 {


  case class ListStack[A](ls : List[A]) {
    def push : A => ListStack[A] =
      a => ListStack(a :: ls)

    def pop : Option[(A, ListStack[A])] =
      ls.headOption map {h => (h, ListStack(ls.tail))}

    def isEmpty : Boolean = ls.isEmpty
  }

  // TODO: Iterable is better abstraction ?
  def foldLeftError[A, B, E] : Traversable[A] => B => ((B, A) => \/[E, B]) => \/[E, B] =
    tr => b => f => tr.foldLeft(\/-(b) : \/[E, B]){
      (eb : \/[E, B], a : A) => eb flatMap ((_b : B) => f(_b, a))
    }

  def solve_with_monads(str: String) : \/[Int, Unit] = {

    val errorIndexOrFinalStack =
      foldLeftError(
        str.toList.zip(List.range(0, str.length))
      )(ListStack[(Char)](List.empty)){ (stack, ci) =>
        val (c, index) = ci
        c match {
          case '(' => \/-(stack.push('('))
          case ')' => {
            stack.pop match {
              case Some((_c, _stack)) => {
                if (_c == '(')
                  \/-(_stack)
                else
                  -\/(index)
              }
              case None => -\/(index)
            }
          }
          case _ => -\/(index)
        }
      }

    errorIndexOrFinalStack flatMap { stack =>
      if (stack.isEmpty)
        \/-(())
      else
        -\/(str.length)
    }
  }


  def solve_by_mutable_array(str: String) : Option[Int] = {
    val stack = new scala.collection.mutable.Stack[Int]
    var index = 0
    for (c <- str) {
      c match {
        case '(' => {
          stack.push('(')
        }
        case ')' => {
          if (stack.isEmpty || (stack.nonEmpty && stack.pop() != '('))
            return Some(index)
        }
        case _   => return Some(index)
      }
      index = index + 1
    }
    if (stack.nonEmpty) return Some(index)
    None
  }
}
