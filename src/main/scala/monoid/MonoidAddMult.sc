import cats.syntax.all._
import algebra.std.string._
import cats.std.int._
//below can also be done as implicit val monoid: Monoid[Int] = Ring[Int].additive
import cats.std.list.listAlgebra
import play.monoid.Eithers


val (a1,a2,a3) = ("Hello","World","!")
import Eithers._

val l = left[String,Int] _
val r = right[String,Int] _


val e1 = l(a1)
val e1b = l(a2)
val e2 = r(1)
val e3 = l(a1 ++ a2)
val e4 = l(a1) ++ l(a2) ++ r(2) ++ r(3)

// a couple of functions from A => Z and B => Z
def p(s: String) = s.toCharArray.toSeq.toList
def f(n: Int): List[Char] = List.fill(n)('-')

val z1 = e1.fold(p,f)
val z1b = e1b.fold(p,f)
val z2 = e2.fold(p,f)
val z3 = e3.fold(p,f)
val z4 = e4.fold(p,f)

val z4bis = p(a1)++p(a2)++f(2)++f(3)