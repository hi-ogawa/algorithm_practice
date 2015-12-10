package net.hiogawa.algorithm_practice.cracking

object _1 {
  def _6 : String => String =
    str => {

      var compStr = ""

      var _keep : Option[(Char, Int)] = None

      str.foreach{ c =>
        _keep match {
          case None =>
            _keep = Some((c, 1))
          case Some((keepC, keepI)) => {
            if (c == keepC) {
              _keep = Some((keepC, keepI + 1))
            } else {
              compStr = compStr + keepC + keepI.toString
              _keep = Some((c, 1))
            }
          }
        }
      }

      _keep.foreach{ case (keepC, keepI) =>
        compStr = compStr + keepC + keepI.toString
      }

      if (str.length < compStr.length) str else compStr
    }
}
