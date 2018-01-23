package stackoverflow

import cats.{Id, ~>}
import myfree.Free

object StackOverflowErrorExample extends App {

  sealed trait Expr[_]

  case class Add(a: Int, b: Int) extends Expr[Int]


  val handler: Expr ~> Id = new (Expr ~> Id) {
    override def apply[A](fa: Expr[A]): Id[A] = fa match {
      case Add(a, b) => a + b
    }
  }

  def addOne(a: Int): Free[Expr, Int] =
    Free.liftM(Add(a, 1))

  def addOneNTimes(a: Int, n: Int): Free[Expr, Int] = {
    def go(expr: Free[Expr, Int], k: Int): Free[Expr, Int] =
      if (k <= 0) expr else go(expr.flatMap(a => addOne(a)), k - 1)

    go(Free.pure(a), n)
  }

  println(addOneNTimes(3, 500).foldMap(handler))

}
