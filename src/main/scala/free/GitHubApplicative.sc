//run these from sbt shell preferably with ammonite
//to reduce calls to server

import cats.~>
import free.{GitHubApplicative => GHA,GHInterpret,GitHub,GitHubAuth}
import org.http4s.client.blaze.PooledHttp1Client

import scalaz.concurrent.Task

//get your token from https://github.com/settings/tokens/new
//see https://developer.github.com/v3/oauth/
val token = "c_"
val client = PooledHttp1Client()

val interpreter: GitHub ~> Task = GHInterpret(client,GitHubAuth(token))


val logins1 = GHA.logins(List("bblfish","markus1189","sjrd","milessabin"))
val logins2 = GHA.logins(List("mgttlinger","tpolecat","DavidGregory084","ceedubs"))
val logins3 = GHA.logins(List("gregghz","InTheNow","non","dialelo"))
val loginsDups = GHA.logins(List("bblfish","tpolecat","non","rossabaker"))


import cats.syntax.applicative._
import cats.syntax.cartesian._
import free.GitHubApplicative._

val logins = (logins1 |@| logins2 |@| logins3 |@| loginsDups)
              .map(_ ++ _ ++ _ ++ _)
val users: List[free.User] = logins.foldMap(interpreter).attemptRun.toOption.get
val users2: List[free.User] = logins.foldMap(interpreter).attemptRun.toOption.get

//if Task is run in a parallel applicative this should not be the same
users == users2

//let's just check the exact order
users.map(_.login)

client.shutdownNow()