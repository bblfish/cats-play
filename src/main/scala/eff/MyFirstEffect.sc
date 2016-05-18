// http://atnos-org.github.io/eff-cats/org.atnos.site.Introduction.html

import cats.data._
import org.atnos.eff._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._

type Stack = Reader[Int, ?] |: Writer[String, ?] |: Eval |: NoEffect

val program: Eff[Stack, Int] = for {
// get the configuration
  n <- ask

  // log the current configuration value
  _ <- tell("the required power is "+n)

  // compute the nth power of 2
  a <- delay(math.pow(2, n.toDouble).toInt)

  // log the result
  _ <- tell("the result is "+a)
} yield a

program.runReader(6).runWriter.runEval.run