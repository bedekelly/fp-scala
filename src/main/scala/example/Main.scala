package example


object Main extends App {

  /**
    * Test whether an array of elements is ordered, given a comparison function.
    * @param as The array of elements to test.
    * @param ordered A comparison function to test whether a pair of elements is ordered.
    * @tparam A The type of elements in our array.
    * @return Whether the array of elements is ordered.
    */
  def isOrdered[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    def aux(n: Int): Boolean = {
      if (n >= as.length-1) true
      else if (!ordered(as(n), as(n+1))) false
      else aux(n+1)
    }
    aux(0)
  }
}
