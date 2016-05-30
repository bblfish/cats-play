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

val logins = (logins1 |@| logins2 |@| logins3) .map(_ ++ _ ++ _)
val users = logins.foldMap(interpreter).attemptRun.toOption.get
val users2 = logins.foldMap(interpreter).attemptRun.toOption.get

users == users2


client.shutdownNow()