package eff

/**
  * Created by hjs on 19/05/2016.
  */
import org.atnos.eff._
import org.atnos.eff.Interpret._
import all._
import cats.data.Xor
import syntax.all._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration

object FutEffect {
  type Fut[A] = Future[() => A]

  def fut[R, A](a: => A)(implicit m: Fut <= R): Eff[R, A] =
    send[Fut, R, A](Future(() => a))

  def runFuture[R <: Effects, U <: Effects, A, B](atMost: Duration)(effects: Eff[R, A])(
    implicit m: Member.Aux[Fut, R, U]): Eff[U, A] = {

    val recurse = new Recurse[Fut, U, A] {
      def apply[X](m: Fut[X]): X Xor Eff[U, A] =
        Xor.Left(Await.result(m.map(_ ()), atMost))
    }
    interpret1((a: A) => a)(recurse)(effects)(m)
  }
}