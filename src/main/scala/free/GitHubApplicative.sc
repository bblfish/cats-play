//run these from sbt shell preferably with ammonite
//to reduce calls to server

import java.io.File
import java.net.URL
import java.nio.file.Files

import com.typesafe.config.ConfigFactory

import scalaz.{Applicative, Functor, Monad, ~>}
import scalaz.Scalaz._
import scalaz.concurrent.{Strategy, Task}
import free._
import org.http4s.client.blaze.PooledHttp1Client

import scalaz.Tags._
import scalaz.concurrent.Task.ParallelTask
import scalaz.syntax.tag._
import free.GitHubApplicative._

import Task.taskParallelApplicativeInstance

//get your token from https://github.com/settings/tokens/new
//see https://developer.github.com/v3/oauth/
//and place it in the file of your choosing and point to it here:
val config = ConfigFactory.parseFile(new File("/Users/hjs/tmp/github.conf"))
val token = config.getString("github.token")


val client = PooledHttp1Client()
val interpreter: GitHub ~> Task = GHInterpret(client,GitHubAuth(token))


def interpretOpt[A](
  p: GHApplicative[A],
  //we can't just be general and have an T: Applicative as otherwise
  //we don't get the flatMap below!
  interp: GitHub ~> Task
): Task[A] = {
  val mapping: ParallelTask[Map[String,User]] =
    precompute(p,interpreter andThen ParallelTaskNat)
  // we need to be able to move from a ParallelTask to a Task to get the
  // flatMap
  mapping.unwrap.flatMap{ m =>
     val betterNat = optimizeNat(m,interp andThen ParallelTaskNat)
     p.foldMap(betterNat).unwrap  //again we want to return a normal Task
  }
}


val logins1 = loginsToApp(List("bblfish", "markus1189", "sjrd", "milessabin"))
val logins2 = loginsToApp(List("mgttlinger", "tpolecat", "DavidGregory084", "ceedubs"))
val loginsDups = loginsToApp(List("bblfish", "tpolecat", "non", "rossabaker"))


val loginsApp: GHApplicative[List[User]] =
  (logins1 ⊛ logins2 ⊛ logins1 ⊛ loginsDups){ _ ++ _ ++ _ ++ _ }

//for loginsTask.runAsync
//val outputF = (either: \/[Throwable,List[User]] ) => either match {
//  case -\/(e) => println(e)
//  case \/-(users) => println(users)
//}
val users1Order = {
  //interpretOpt should have made a bunch of parallele tasks
  val parrallelTask: Task[List[User]] = interpretOpt(loginsApp, interpreter)

  val users1 = parrallelTask.unsafePerformSync
  users1.map(_.login)
}

val users2Order = {
  val users2 = loginsApp.foldMap(interpreter).unsafePerformSync
  users2.map(_.login)
}
//running tasks in parallel and synchronously gives the same results,
//and the optimized one gives the same results as the non optimized one
users1Order == users2Order

//
//val reposIssues: GHApplicative[List[List[Issue]]] =
//  List("rww-play","rww-scala-js").traverseU(repo=>
//    listIssues("read-write-web",repo)
//  )
//val issuesAp = reposIssues.map(_.flatten)
//val issueParTask = issuesAp.foldMap(interpreter andThen ParallelTaskNat)
//val issues = issueParTask.unwrap.unsafePerformSync

client.shutdownNow()
