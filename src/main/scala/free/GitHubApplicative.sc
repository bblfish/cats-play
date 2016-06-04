//run these from sbt shell preferably with ammonite
//to reduce calls to server

import scalaz.~>
import scalaz.Scalaz._
import scalaz.concurrent.{Strategy, Task}
import free._
import org.http4s.client.blaze.PooledHttp1Client

import scalaz.Tags._
import scalaz.concurrent.Task.ParallelTask

//get your token from https://github.com/settings/tokens/new
//see https://developer.github.com/v3/oauth/
val token = "c_"
val client = PooledHttp1Client()


val interpreter: GitHub ~> Task = GHInterpret(client,GitHubAuth(token))

import free.GitHubApplicative._

val logins1 = loginsToApp(List("bblfish", "markus1189", "sjrd", "milessabin"))
val logins2 = loginsToApp(List("mgttlinger", "tpolecat", "DavidGregory084", "ceedubs"))
val logins3 = loginsToApp(List("gregghz", "InTheNow", "non", "dialelo"))
val loginsDups = loginsToApp(List("bblfish", "tpolecat", "non", "rossabaker"))


val loginsApp: GHApplicative[List[User]] =
  (logins1 ⊛ logins2 ⊛ logins3 ⊛ loginsDups){ _ ++ _ ++ _ ++ _ }

//for loginsTask.runAsync
//val outputF = (either: \/[Throwable,List[User]] ) => either match {
//  case -\/(e) => println(e)
//  case \/-(users) => println(users)
//}

import scalaz.syntax.tag._

val parrallelTask: ParallelTask[List[User]] = loginsApp.foldMap(interpreter andThen ParallelTaskNat)(Task.taskParallelApplicativeInstance)

val users1 = parrallelTask.unwrap.unsafePerformSync
val users2 = loginsApp.foldMap(interpreter).unsafePerformSync
//if Task is run in a parallel applicative this should not be the same
users1 == users2

//let's just check the exact order
val users1Order = users1.map(_.login)
val users2Order = users1.map(_.login)

client.shutdownNow()