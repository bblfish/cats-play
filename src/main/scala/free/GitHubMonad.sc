//run these from sbt shell preferably with ammonite
//to reduce calls to server

import cats.~>
import free.{GitHubApplicative => GHA, GitHubMonadic => GHM, _}
import org.http4s.client.blaze.PooledHttp1Client

import scalaz.concurrent.Task

//get your token from https://github.com/settings/tokens/new
//see https://developer.github.com/v3/oauth/
val token = "c_"
val client = PooledHttp1Client()

val interpreter: GitHub ~> Task = GHInterpret(client,GitHubAuth(token))

interpreter(GetUser("bblfish")).attemptRun
interpreter(ListIssues("read-write-web","rww-play")).attemptRun
interpreter(ListIssues("read-write-web","rww-play")).attemptRun
interpreter(GetComments("read-write-web","rww-play",155)).attemptRun


val endTask = GHM.allUsers("read-write-web","rww-play").foldMap(interpreter)
val resOr = endTask.attemptRun


client.shutdownNow()