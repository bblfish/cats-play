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

task(GetUser("bblfish")).attemptRun
task(ListIssues("read-write-web","rww-play")).attemptRun
task(ListIssues("read-write-web","rww-play")).attemptRun
task(GetComments("read-write-web","rww-play",155)).attemptRun


val endTask = GHM.allUsers("read-write-web","rww-play").foldMap(task)
val resOr = endTask.attemptRun


client.shutdownNow()