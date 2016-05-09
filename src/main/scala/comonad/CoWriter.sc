import comonad.FunkyOO._
import cats._
import cats.implicits._
//if one does not import the cats.implicits, one has to look at least in the next two imports
//import cats.syntax.comonad._
//import cats.std...

// http://www.haskellforall.com/2013/02/you-could-have-invented-comonads.html
// here using to co-monad cats classes to create
//a coWriter following
// http://blog.higher-order.com/blog/2015/06/23/a-scala-comonad-tutorial/

case class Config(private val options: Opts)

type Opt = String
type Opts = List[Opt]
type Builder[A] = Opts => A
type BuilderC = Builder[Config]
type Setter[A,B] = Builder[A] => B
type SetterC = Setter[Config,Config]
type Appender[A] = Builder[A] => Builder[A]

//The Builder example is a cowriter

object X {
  type CoWriter[M,A] = M=>A
  implicit def coWriter[M:Monoid,A]: Comonad[CoWriter[M,?]] =
    new Comonad[CoWriter[M,?]] {
      override
      def extract[A](builder: CoWriter[M, A]): A = builder(Monoid[M].empty)
      //the cats name for `extend`
      override
      def coflatMap[A, B](builder: CoWriter[M, A])(setter: (CoWriter[M, A]) => B): CoWriter[M, B] =
        (opts2: M) => setter( (opts: M) => builder(Monoid[M].combine(opts, opts2)))
      override
      def map[A, B](builder: CoWriter[M, A])(f: (A) => B): CoWriter[M, B] = (opts: M) => f(extract(builder))
    }


}

import X._

val default: BuilderC = (opts: Opts) => Config("-Wall"::opts)
val profile: SetterC = (b: BuilderC) =>  b(List("-prof","-auto-all"))

val goFaster: SetterC = (b: BuilderC) => b(List("-O2"))

default.extract
profile(default)
goFaster(default)

default.coflatMap(goFaster).coflatMap(profile).extract






