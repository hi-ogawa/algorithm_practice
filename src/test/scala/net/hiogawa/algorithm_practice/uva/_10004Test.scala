package net.hiogawa.algorithm_practice.uva

import org.specs2.mutable._

class _10004Test extends Specification {

  "getOneGraphInput" >> {
    val input =
      """3
3
0 1
1 2
2 0
"""
    val lines = input.lines
    Utils.enumerator(lines)(_10004.getOneGraphInput) must be_==(
      (
        3,
        List(
          (0, 1),
          (1, 2),
          (2, 0)
        )
      )
    )
  }

  "Graph.dfs_connected" >> {
    val g = _10004.mkGraph(
      3,
      List(
        (0, 1),
        (1, 2),
        (2, 0)
      )
    )

    g.dfs_connected
    true
  }

  "Graph.dfs_colorable" >> {
    val g = _10004.mkGraph(
      3,
      List(
        (0, 1),
        (1, 2),
        (2, 0)
      )
    )

    g.dfs_colorable must be_==(false)
  }

  "solve" >> {
    _10004.solveFromString(
      """3
3
0 1
1 2
2 0
3
2
0 1
1 2
9
8
0 1
0 2
0 3
0 4
0 5
0 6
0 7
0 8
0
"""
    ) must be_==(
      """NOT BICOLORABLE.
BICOLORABLE.
BICOLORABLE.
"""
    )
  }

  "solve hard" >> {
    val input = scala.io.Source.fromURL(getClass.getResource("/10004.input")).mkString
    val output = scala.io.Source.fromURL(getClass.getResource("/10004.output")).mkString
    _10004.solveFromString(input) must be_==(output)
  }
}
