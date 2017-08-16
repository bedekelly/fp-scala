package example


sealed trait MyList[+A] {
  def sum[B >: A](implicit num: Numeric[B]): B = MyList.sum[B](this)
}
case object MyNil extends MyList[Nothing]
case class MyCons[+A](h: A, t: MyList[A]) extends MyList[A]


object MyList {
  def sum[A](ints: MyList[A])(implicit num: Numeric[A]): A = ints match {
    case MyNil => num.zero
    case MyCons(x, xs) => num.plus(x, sum(xs))
  }

  def product(ds: MyList[Double]): Double = ds match {
    case MyNil => 1.0
    case MyCons(0.0, _) => 0.0
    case MyCons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): MyList[A] =
    if (as.isEmpty) MyNil
    else MyCons(as.head, apply(as.tail: _*))
}


object Main extends App {

}
