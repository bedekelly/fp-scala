package example

import org.scalatest._

class MainSpec extends FlatSpec with Matchers {
  "the tail method" should "return the tail of a list" in {
    val as: MyList[Int] = MyList(1, 2, 3)
    MyList.tail(as) shouldEqual MyList(2, 3)
  }

  "the tail method" should "return Nil for an empty list" in {
    val as: MyList[Int] = MyList()
    MyList.tail(as) shouldEqual MyList()
  }

  "the drop method" should "remove the first n elements from a list" in {
    val as: MyList[Int] = MyList(1, 2, 3, 4, 5)
    MyList.drop(as, 2) shouldEqual MyList(3, 4, 5)
  }

  "the drop method" should "return an empty list given n > list items" in {
    val as: MyList[Int] = MyList(1, 2, 3)
    MyList.drop(as, 10) shouldEqual MyList()
  }

  "the dropwhile method" should "return only as many elements as match a predicate" in {
    val as: MyList[Int] = MyList(2, 4, 6, 8, 1, 3, 5, 7)
    def isEven(x: Int): Boolean = x % 2 == 0
    MyList.dropWhile(as, isEven) shouldEqual MyList(2, 4, 6, 8)
  }

  "the dropwhile method" should "return no elements if the first element does not match the predicate" in {
    val as: MyList[Int] = MyList(1, 3, 5)
    def isEven(x: Int): Boolean = x % 2 == 0
    MyList.dropWhile(as, isEven) shouldEqual MyList()
  }

  "the dropwhile method" should "return all elements if all elements match the predicate" in {
    val as: MyList[Int] = MyList(1, 2, 3, 4, 5)
    def smallEnough(x: Int): Boolean = x < 10
    MyList.dropWhile(as, smallEnough) shouldEqual MyList(1, 2, 3, 4, 5)
  }
}
