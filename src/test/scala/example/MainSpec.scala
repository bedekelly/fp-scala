package example

import org.scalatest._

class MainSpec extends FlatSpec with Matchers {

  def lt(a: Int, b: Int): Boolean = a < b

  "The isOrdered method" should "correctly identify ordered arrays" in {
    val ordered = Array(1, 2, 3, 4, 5, 6, 8, 10, 35, 100, 53035)

    Main.isOrdered(ordered, lt) shouldEqual true
  }

  "The isOrdered method" should "correctly identify unordered arrays" in {
    val unordered = Array(32, 32, 31, 53, 53, 90, 10000)
    Main.isOrdered(unordered, lt) shouldEqual false
  }

  "The isOrdered method" should "categorise empty arrays as ordered" in {
    val empty = Array()
    Main.isOrdered(empty, lt) shouldEqual true
  }

  "The isOrdered method" should "categorise single-element arrays as ordered" in {
    val singleElement = Array(1)
    Main.isOrdered(singleElement, lt) shouldEqual true
  }
}
