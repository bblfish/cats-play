import cats.syntax.all._
import algebra.std.string._
import cats.std.int._
import play.monoid.Eithers

// implicit val monoid: Monoid[Int] = Ring[Int].additive


val (a1,a2,a3) = ("Hello","World","!")
import Eithers._

val l = left[String,Int] _
def r = right[String,Int] _


l(a1)
r(1)
l(a1 ++ a2)
l(a1) ++ l(a2) ++ r(2) ++ r(5)