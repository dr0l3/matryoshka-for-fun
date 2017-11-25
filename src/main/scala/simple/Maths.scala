package simple

import matryoshka._
import matryoshka.data.Fix
import matryoshka.implicits._

import scala.annotation.tailrec
import scalaz.Functor

object Maths extends App {

  sealed trait Expr[A]
  case class Num[A](v: Int) extends Expr[A]
  case class Mul[A](x: A, y: A) extends Expr[A]
  case class Add[A](x: A, y: A) extends Expr[A]

  implicit val exprFunctor: Functor[Expr] = new Functor[Expr] {
    override def map[A, B](fa: Expr[A])(f: A => B): Expr[B] = fa match {
      case Num(v) => Num(v)
      case Mul(x,y) => Mul(f(x),f(y))
      case Add(x,y)=> Add(f(x), f(y))
    }
  }


  def num[A](v: Int): Fix[Expr] = Fix(Num(v))
  def mul[A](x: Fix[Expr], y: Fix[Expr]): Fix[Expr] = Fix(Mul(x,y))

  val show: Algebra[Expr,String] = {
    case Num(v) => s"$v"
    case Mul(x,y) => s"($x * $y)"
    case Add(x,y) => s"($x + $y)"
  }

  val eval: Algebra[Expr,Int] = {
    case Num(v)=> v
    case Mul(x,y) => x * y
    case Add(x,y) => x + y
  }

  val parse: Coalgebra[Expr, Int] = {
    case n if n == 1 => Num(1)
    case n if n == 2 => Add(1,1)
    case n if n % 2 == 0 => Mul(2,n/2)
    case n => Add(1, n-1)
  }

  val thirty = mul(num(15),mul(num(1),num(2)))
  println(thirty.cata(show))
  println(thirty.cata(eval))
  println(467.ana[Fix[Expr]](parse).cata(show))
  println(20.ana[Fix[Expr]](parse))

  @tailrec
  def parseExplicitRecursion(number: Int, acc: String, append: String): String = number match {
    case n if n == 1 => acc + s"1" + append
    case n if n == 2 => acc + s"(1 + 1)" + append
    case n if n % 2 == 0 =>
      val next = n/2
      parseExplicitRecursion(next,acc + s"((1 + 1) * ", append + ")")
    case n =>
      val next = n -1
      parseExplicitRecursion(next, acc + s"(1 + ", append + ")")
  }

  println(parseExplicitRecursion(467,"",""))
}
