package example


sealed trait MyList[+A]
case object MyNil extends MyList[Nothing]
case class MyCons[+A](h: A, t: MyList[A]) extends MyList[A]


object MyList {
  def apply[A](as: A*): MyList[A] =
    if (as.isEmpty) MyNil
    else MyCons(as.head, apply(as.tail: _*))

  def tail[A](as: MyList[A]): MyList[A] = as match {
    case MyNil => MyNil
    case MyCons(_, x) => x
  }

  def drop[A](as: MyList[A], n: Int): MyList[A] = (n, as) match {
    case (0, x) => x
    case (_, MyCons(x, xs)) => drop(xs, n-1)
    case (_, MyNil) => MyNil
  }

  def dropWhile[A](as: MyList[A], pred: A => Boolean): MyList[A] = as match {
    case MyNil => MyNil
    case MyCons(x, xs) =>
      if (pred(x)) MyCons(x, dropWhile(xs, pred))
      else MyNil
  }

  def append[A](a1: MyList[A], a2: MyList[A]): MyList[A] =
    a1 match {
      case MyNil => a2
      case MyCons(x, xs) => MyCons(x, append(xs, a2))
    }

  def init[A](as: MyList[A]): MyList[A] = as match {
    case MyCons(x, MyCons(y, ys)) => MyCons(x, init(MyCons(y, ys)))
    case MyCons(x, MyNil) => MyNil
    case MyNil => MyNil
  }

  def foldRight[A, B](as: MyList[A], z: B)(f: (A, B) => B): B =
    as match {
      case MyNil => z
      case MyCons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def product2(ns: MyList[Double]): Double =
    foldRight(ns, 1.0)(_ * _)

  def sum2(ns: MyList[Int]): Int =
    foldRight(ns, 0)(_ + _)

  def length[A](as: MyList[A]): Int =
    foldRight(as, 0)((_, n) => n+1)
}


object Main extends App {

}
