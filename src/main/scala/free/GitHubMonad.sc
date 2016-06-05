//run these from sbt shell preferably with ammonite
//to reduce calls to server

import java.io.File

import cats.~>
import com.typesafe.config.ConfigFactory
import free.{GitHubApplicative => GHA, GitHubMonadic => GHM, _}
import org.http4s.client.blaze.PooledHttp1Client

import scalaz.concurrent.Task

//get your token from https://github.com/settings/tokens/new
//see https://developer.github.com/v3/oauth/
//and place it in the file of your choosing and point to it here:
val config = ConfigFactory.parseFile(new File("/Users/hjs/tmp/github.conf"))
val token = config.getString("github.token")
val client = PooledHttp1Client()

val interpreter: GitHub ~> Task = GHInterpret(client,GitHubAuth(token))

interpreter(GetUser("bblfish")).attemptRun
interpreter(ListIssues("read-write-web","rww-play")).attemptRun
interpreter(ListIssues("read-write-web","rww-play")).attemptRun
interpreter(GetComments("read-write-web","rww-play",155)).attemptRun


val endTask = GHM.allUsers("read-write-web","rww-play").foldMap(interpreter)
val resOr = endTask.attemptRun


client.shutdownNow()