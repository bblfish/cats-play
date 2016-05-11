//from http://www.casualmiracles.com/2012/07/02/a-small-example-of-kleisli-arrows/

import cats.data.Kleisli
import cats.std.option._
import cats.syntax.all._

def str(x: Int): Option[String] = Some(x.toString)
def toInt(x: String): Option[Int] = Some(x.toInt)
def double(x: Int): Option[Double] = Some(x * 2)

def oldSchool(i: Int) =
  for {x <- str(i)
       y <- toInt(x)
       z <- double(y)}
    yield z

//Cats does not have fishy operators >=>
val funky = Kleisli(str _) andThen (toInt _) andThen (double _)

oldSchool(3)
funky(3)