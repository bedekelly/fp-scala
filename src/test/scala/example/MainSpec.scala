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

  "the append method" should "join two lists together" in {
    val as: MyList[Int] = MyList(1, 2, 3)
    val bs: MyList[Int] = MyList(4, 5, 6)
    MyList.append(as, bs) shouldEqual MyList(1, 2, 3, 4, 5, 6)
  }

  "the append method" should "join an empty list to a populated list by returning the populated list" in {
    val as: MyList[Int] = MyList()
    val bs: MyList[Int] = MyList(1, 2, 3)
    MyList.append(as, bs) shouldEqual MyList(1, 2, 3)
  }

  "the append method" should "join a populated list to an empty list by returning the populated list" in {
    val as: MyList[Int] = MyList(1, 2, 3)
    val bs: MyList[Int] = MyList()
    MyList.append(as, bs) shouldEqual MyList(1, 2, 3)
  }

  "the init method" should "return all but the last elements of a list" in {
    val as: MyList[Int] = MyList(1, 2, 3, 4, 5, 6)
    MyList.init(as) shouldEqual MyList(1, 2, 3, 4, 5)
  }

  "the init method" should "return Nil for an empty list" in {
    val as: MyList[Int] = MyList()
    MyList.init(as) shouldEqual MyList()
  }

  "the init method" should "return Nil for a list of one item" in {
    val as: MyList[Int] = MyList(1)
    MyList.init(as) shouldEqual MyList()
  }

  "the sum2 method" should "correctly sum a list" in {
    val as: MyList[Int] = MyList(1, 2, 3, 4, 5)
    MyList.sum2(as) shouldEqual 15
  }

  "the sum2 method" should "sum an empty list to 0" in {
    val as: MyList[Int] = MyList()
    MyList.sum2(as) shouldEqual 0
  }

  "the product2 method" should "correctly calculate the product of a list" in {
    val as: MyList[Double] = MyList(1, 2, 3, 4, 5)
    MyList.product2(as) shouldEqual 120
  }

  "the product2 method" should "calculate the product of an empty list as 1" in {
    val as: MyList[Double] = MyList()
    MyList.product2(as) shouldEqual 1
  }

  "the length method" should "calculate the length of an empty list as 0" in {
    val as: MyList[Double] = MyList()
    MyList.length(as) shouldEqual 0
  }

  "the length method" should "correctly calculate the length of any list" in {
    val as: MyList[String] = MyList("a", "b", "c", "d", "e")
    MyList.length(as) shouldEqual 5
  }

  "the foldLeft sum method" should "correctly sum a list" in {
    val as: MyList[Int] = MyList(1, 2, 3, 4, 5)
    MyList.sumLeft(as) shouldEqual 15
  }

  "the foldLeft sum method" should "sum an empty list to zero" in {
    val as: MyList[Int] = MyList()
    MyList.sumLeft(as) shouldEqual 0.0
  }

  "the foldLeft length method" should "count a list's length correctly" in {
    val as: MyList[Int] = MyList(1, 2, 3, 2)
    MyList.lengthLeft(as) shouldEqual 4
  }
}
