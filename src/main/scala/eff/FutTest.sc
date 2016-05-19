import eff.FutEffect._

import org.atnos.eff._
import all._
import scala.concurrent.duration._



type F = Fut |: NoEffect

val action: Eff[F, Int] = for {
  a <- fut(2)
  b <- fut(3)
} yield a + b

run(runFuture(3.seconds)(action))