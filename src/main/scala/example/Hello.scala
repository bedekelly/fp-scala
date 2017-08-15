package example

import scala.annotation.tailrec


object Hello {

  def findFirstString(key: String, ss: Array[String]): Int = {
    @tailrec
    def aux(n: Int): Int =
      if (n >= ss.length) -1
      else if (ss(n) == key) n
      else aux(n + 1)

    aux(0)
  }

  def findFirst[A](p: A => Boolean, as: Array[A]): Int = {
    @tailrec
    def aux(n: Int): Int =
      if (n >= as.length) -1
      else if (p(as(n))) n
      else aux(n+1)

    aux(0)
  }

  def main(args: Array[String]): Unit = {
    val ss = Array("a", "bb", "ccc", "dddd")
    println(findFirstString("ccc", ss))
    println(findFirst[String](x => x == "eeeee", ss))
  }
}

