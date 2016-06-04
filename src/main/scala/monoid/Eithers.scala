package play.monoid

import scalaz.Monoid
import scalaz.Scalaz._

//code from http://blog.higher-order.com/blog/2014/03/19/monoid-morphisms-products-coproducts/

object Eithers {
  def left[A: Monoid, B: Monoid](a: A): Eithers[A, B] =
    Eithers(List(Left(a)))

  def right[A: Monoid, B: Monoid](b: B): Eithers[A, B] =
    Eithers(List(Right(b)))

  def empty[A: Monoid, B: Monoid]: Eithers[A, B] = new Eithers(Nil)

  def apply[A: Monoid, B: Monoid](xs: List[Either[A, B]]): Eithers[A, B] =
    new Eithers(xs.foldRight(List[Either[A, B]]()) {
      case (Left(a1), Left(a2) :: xs) => Left(a1 |+| a2) :: xs
      case (Right(b1), Right(b2) :: xs) => Right(b1 |+| b2) :: xs
      case (e, xs) => e :: xs
    })
}

sealed class Eithers[A: Monoid, B: Monoid](
  private val toList: List[Either[A, B]]
) {

  def ++(p: Eithers[A, B]): Eithers[A, B] =
    Eithers(toList ++ p.toList)

  def fold[Z: Monoid](f: A => Z, g: B => Z): Z =
    toList.foldRight(Monoid[Z].zero) {
      case (Left(a), z) => f(a) |+| z
      case (Right(b), z) => g(b) |+| z
    }

  override
  def toString() = toList.toString()
}
