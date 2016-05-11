// http://blog.higher-order.com/blog/2015/06/23/a-scala-comonad-tutorial/

import cats._
import cats.data.Reader
import cats.implicits._
import cats.data.Kleisli._


val rdr = for {
  r <- ask[Id,Int]
  r2 <- pure[Id,Int,Int](5)
  r3 <-
} yield { println(s"$r*$r2+7* $r3");(r*r2)+r3 }

rdr.run(3)