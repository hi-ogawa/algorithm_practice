package net.hiogawa.algorithm_practice.uva

import scala.language.postfixOps
import scalaz._

// Problem: https://uva.onlinejudge.org/index.php?option=com_onlinejudge&Itemid=35&category=37&page=show_problem&problem=945
object _10004 {
  // NOTE: graph is bicolorable <-> graph is bipertite
  //  - start from any vertex and traverse a whole graph (preferrably dfs for simplicity reason)
  //    with labelling each vertex alternately
  //  - if you find conflict, then not bicolorable, else bicolorable

  // References:
  //  - http://docs.scala-lang.org/overviews/collections/iterators.html#buffered-iterators
  //  - http://www.scala-lang.org/api/current/index.html#scala.Function2
  //  - http://eed3si9n.com/learning-scalaz/Lens.html
  //  - http://docs.scala-lang.org/overviews/core/string-interpolation.html

  // TODO:
  //  - each graph problem should be solved in parallell. Future could be it.
  //  - think about the way everything is done one Iterator
  //  - use existing iteratee library from play framework
  //  - use existing graph library
  //  - create github repository

  def solveFromString (input: String) : String = {

    val lines : BufferedIterator[String] = input.lines.buffered

    var answers = scala.collection.mutable.Stack.empty[Boolean]

    while (lines.head != "0") {
      val g : Graph = Utils.enumerator(lines)(getOneGraphInput.map(mkGraph _ tupled))
      answers.push(solveOne(g))
    }

    answers.reverse.map{
      case true  => "BICOLORABLE.\n"
      case false => "NOT BICOLORABLE.\n"
    }.foldLeft("")((a, b) => a + b)
  }

  def getOneGraphInput : Utils.Iteratee[String, (Int, List[(Int, Int)])] = {
    for {
      nVertices <- Utils.takeOne[String].map{_.toInt}
      nEdges <- Utils.takeOne[String].map{_.toInt}
      ls <- Utils.takeN(nEdges)
    } yield {
      (
        nVertices,
        ls.map{ l : String =>
          val ar = l.split(' ').map(_.toInt)
          (ar(0), ar(1))
        }
      )
    }
  }

  case class Graph(nodes : Array[Node]) {
    def addEdge(edge : (Int, Int)) : Unit = {
      nodes(edge._1) = nodeAdjs.mod((edge._2 :: _), nodes(edge._1))
      nodes(edge._2) = nodeAdjs.mod((edge._1 :: _), nodes(edge._2))
    }

    def isVisited(i : Int) : Boolean = nodes(i).visited

    def markVisited(i : Int) : Unit = nodes(i) = nodeVisited.set(nodes(i), true)

    def putColor(i : Int, c: Int) : Unit = nodes(i) = nodeColor.set(nodes(i), Some(c))

    def isColored(i : Int) : Boolean = nodes(i).color.isDefined

    def isSameColor(i : Int, j: Int) : Boolean =
      isColored(i) && isColored(j) && nodes(i).color == nodes(j).color

    // TODO: what to return from dfs (consult skine's book)
    def dfs_connected : Unit = {
      dfs_sub(
        0,
        None,
        (to, _from) => println(s"${to} <- ${_from} (first time)"),
        (to, _from) => println(s"${to} <- ${_from} (not first time)")
      )
    }

    def dfs_colorable : Boolean = {

      var colorable = true

      val sub : (Int, Option[Int]) => Unit =
        (to, _from) => {
          _from.map{ from =>
            if (isColored(to)) {
              if (isSameColor(to, from))
                colorable = false
            } else {
              putColor(to, ((nodes(from).color.get + 1) % 2))
            }
          }.getOrElse{
            // NOTE: use arbitrary color only for root
            putColor(to, 0)
          }
        }

      dfs_sub(
        0,
        None,
        sub,
        sub
      )
      colorable
    }

    // TODO: passing Effectful routine doesn't look good but easy to implement/debug (not easy to maintain?)
    def dfs_sub(
      to : Int,
      _from : Option[Int],
      sub0 : (Int, Option[Int]) => Unit,
      sub1 : (Int, Option[Int]) => Unit
    ) : Unit = {
      if (isVisited(to)) {
        sub1(to, _from)
      } else {
        sub0(to, _from)
        markVisited(to)
        nodes(to).adjs.foreach{ nextTo =>
          dfs_sub(nextTo, Some(to), sub0, sub1)
        }
      }
    }
  }
  case class Node(label : Int, adjs: List[Int], visited: Boolean, color: Option[Int])
  // NOTE: generate Lenses
  val nodeLabel = Lens.lensu[Node, Int](
    (a, value) => a.copy(label = value),
    _.label
  )
  val nodeAdjs = Lens.lensu[Node, List[Int]](
    (a, value) => a.copy(adjs = value),
    _.adjs
  )
  val nodeVisited = Lens.lensu[Node, Boolean](
    (a, value) => a.copy(visited = value),
    _.visited
  )
  val nodeColor = Lens.lensu[Node, Option[Int]](
    (a, value) => a.copy(color = value),
    _.color
  )

  def mkGraph (nVertices : Int, edges : List[(Int, Int)]) : Graph = {
    var g = Graph(
      Array.range(0, nVertices).map{ n =>
        Node(n, List.empty, false, None)
      }
    )
    edges.foreach(g.addEdge(_))
    g
  }

  def solveOne (g: Graph) : Boolean = g.dfs_colorable

}


object Utils {

  // NOTE: http://stackoverflow.com/questions/3032771/scalas-sealed-abstract-vs-abstract-class
  sealed abstract class Iteratee[I, O] {
    def flatMap[P] : (O => Iteratee[I, P]) => Iteratee[I, P] =
      h => {
        this match {
          case Done(o) => h(o)
          case Cont(f) => {
            Cont{ i : I =>
              f(i).flatMap(h)
            }
          }
        }
      }

    def map[P] : (O => P) => Iteratee[I, P] =
      h => {
        this match {
          case Done(o) => Done(h(o))
          case Cont(f) => {
            Cont{ i : I =>
              f(i).map(h)
            }
          }
        }
      }
  }
  case class Done[I, O](o : O) extends Iteratee[I, O]
  case class Cont[I, O](f : I => Iteratee[I, O]) extends Iteratee[I, O]

  def takeOne[I] : Iteratee[I, I] = {
    Cont{ i : I => Done(i) }
  }

  def takeN[I] : Int => Iteratee[I, List[I]] =
    n => {
      // NOTE: this is just `sequence` in Haskell
      List.range(0, n).map(_ => takeOne[I])
      .foldLeft[Iteratee[I, List[I]]](Done(List.empty)){ (acc, one) =>
        for {
          ls <- acc
          x  <- one
        } yield { x :: ls }
      }
      .map(_.reverse)
    }

  // NOTE: none pure for Iterator
  def enumerator[I, O] : Iterator[I] => Iteratee[I, O] => O =
    ior => iee => {
      iee match {
        case Done(o) => o
        case Cont(f) => {
          var x = ior.next()
          enumerator(ior)(f(x))
        }
      }
    }
}
