package com.github.inaaaaaa

import org.scalatest._

class LifeGameSpec extends FlatSpec with Matchers {
  "The living cell" should "be printed by at sign" in {
    Cell(true).format() shouldEqual "@"
  }
}
