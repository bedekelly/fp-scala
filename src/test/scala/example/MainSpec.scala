package example

import org.scalatest._

class MainSpec extends FlatSpec with Matchers {
  "the product method" should "calculate the product of [] as 1" in {
    val as: MyList[Double] = MyList()
    MyList.product(as) shouldEqual 1
  }

  "the product method" should "correctly calculate the product of a nonempty list" in {
    val as: MyList[Double] = MyList(2, 3, 4, 5, 6)
    MyList.product(as) shouldEqual (2 * 3 * 4 * 5 * 6)
  }

  "the sum method" should "calculate the sum of [] as 0" in {
    val as: MyList[Int] = MyList()
    MyList.sum(as) shouldEqual 0
  }

  "the sum method" should "correctly calculate the sum of a nonempty list" in {
    val as: MyList[Int] = MyList(1, 2, 3, 4, 5, 6)
    MyList.sum(as) shouldEqual (1 + 2 + 3 + 4 + 5 + 6)

    // This next line causes a compile error: there's no implicit evidence that a string is a numeric!
    // MyList("a", "b").sum

  }
}
