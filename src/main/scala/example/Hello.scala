package example

import scala.annotation.tailrec


object Hello {

  /**
    * Calculate the absolute value of a number.
    * @param n The number to calculate.
    * @return The absolute value of the number.
    */
  def abs(n: Int): Int = if (n<0) -n else n

  /**
    * Calculate a number's factorial.
    * @param n The number to calculate.
    * @return n!, n factorial.
    */
  def fact(n: Int): Int = {
    @tailrec
    def aux(n: Int, acc: Int): Int = {
      if (n > 1) aux(n-1, acc*n)
      else acc
    }
    aux(n, 1)
  }

  /**
    * Format the result of applying a function to some integer input.
    * @param name A human-readable name for the function.
    * @param fn The function to apply.
    * @param input The input to feed the function.
    * @return A string in the form "The (function name) of (input) is (output)".
    */
  private def formatResult(name: String, fn: Int => Int, input: Int): String =
    "The %s of %d is %d".format(
      name, input, fn(input)
    )

  def main(args: Array[String]): Unit = {
    println(formatResult("factorial", fact, 5))
    println(formatResult("abs", abs, -36))
  }
}

