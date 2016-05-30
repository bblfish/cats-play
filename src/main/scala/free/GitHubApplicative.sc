//run these from sbt shell preferably with ammonite
//to reduce calls to server

import cats.free.Free
import cats.{Applicative, Monad, ~>}
import free.{GitHubMonadic => GHM}
import free.{GitHubApplicative => GHA}
import free._
import org.http4s.client.blaze.PooledHttp1Client

import scalaz.Nondeterminism
import scalaz.concurrent.Task

//get your token from https://github.com/settings/tokens/new
//see https://developer.github.com/v3/oauth/
val token = "c_"
val client = PooledHttp1Client()

val task: GitHub ~> Task = GHInterpret(client,GitHubAuth(token))


val logins1 = GHA.logins(List("bblfish","markus1189","sjrd","milessabin"))
val logins2 = GHA.logins(List("mgttlinger","tpolecat","DavidGregory084","ceedubs"))
val logins3 = GHA.logins(List("gregghz","InTheNow","non","dialelo"))


import cats.syntax.applicative._
import cats.syntax.cartesian._

import free.GitHubApplicative._

val logins = (logins1 |@| logins2 |@| logins3) .map(_ ++ _ ++ _)
val users = logins.foldMap(task).attemptRun.toOption.get
val users2 = logins.foldMap(task).attemptRun.toOption.get

users == users2


client.shutdownNow()