package simple

import matryoshka._
import matryoshka.data._
import matryoshka.implicits._
import scalaz.Functor

object Introduction extends App {

  sealed trait Nat[A]

  case class Zero[A]() extends Nat[A]
  case class Succ[A](n: A) extends Nat[A]

  def zero(): Fix[Nat] = Fix(Zero())
  def succ(pred: Fix[Nat]):Fix[Nat] = Fix(Succ(pred))

  implicit val natFunctor: Functor[Nat] = new Functor[Nat] {
    def map[A, B](nat: Nat[A])(f: A => B): Nat[B] = nat match {
      case Zero() => Zero()
      case Succ(a) => Succ(f(a))
    }
  }

  val toNat: Coalgebra[Nat, Int] = {
    case 0 => Zero()
    case n => Succ(n - 1)
  }

  val toInt: Algebra[Nat, Int] = {
    case Zero() => 0
    case Succ(n) => n + 1
  }

  val show: Algebra[Nat,String] = {
    case Zero() => "0"
    case Succ(n) => s"succ($n)"
  }

  val factorial: GAlgebra[(Int, ?), Nat, Int] = {
    case Zero() => 1
    case Succ((i, n)) => (i + 1) * n
  }

  val something: GAlgebra[(String,?), Nat, String] = {
    case Zero() => "Big Z"
    case Succ((i,n)) => s"<i = $i | n = $n>" //i + " " + n
  }

  val res =2.hylo(toInt,toNat)
  val res2 =2.ana[Fix[Nat]](toNat)
  val res3 = 4.ana[Fix[Nat]](toNat).cata(toInt)
  val res4 = 5.ana[Fix[Nat]](toNat).zygo(toInt, factorial)
  val res5 = 5.ana[Mu[Nat]](toNat).zygo(show, something)

  println(res)
  println("---")
  println(res2)
  println("---")
  println(res3)
  println("---")
  println(res4)
  println("---")
  println(res5)
  println("---")
  println(5.ana[Nu[Nat]](toNat).cata(show))
  println(succ(succ(succ(zero()))))
  println(succ(succ(succ(zero()))).cata(show))
  println(3.ana[Fix[Nat]](toNat))
}
