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

}


object Main extends App {

}
