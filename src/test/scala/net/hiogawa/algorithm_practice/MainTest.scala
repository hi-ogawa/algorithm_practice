package net.hiogawa.algorithm_practice

import org.specs2.mutable._

class MainTest extends Specification {
  "Main" >> {
    ".testString" >> {
      Main.testString must contain("my package")
    }
  }
}
