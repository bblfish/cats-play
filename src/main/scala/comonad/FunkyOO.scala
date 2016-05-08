package comonad

/**
  * Created by hjs on 06/05/2016.
  */
object FunkyOO {

  /**
    * Functional OO notation as described in
    * http://www.haskellforall.com/2013/02/you-could-have-invented-comonads.html
    * This allows one to look at OO programming from a functional programming perspective.
    *
    * Uses Value classes, see http://docs.scala-lang.org/overviews/core/value-classes.html
    */
  implicit class FO[A](val a: A) extends AnyVal {
    def °[B](f: A => B): B = f(a)
  }

}