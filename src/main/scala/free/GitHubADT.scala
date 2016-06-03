package free

import _root_.argonaut.{CodecJson, _}
import _root_.argonaut.Argonaut._

import scalaz.{-\/, Applicative, Free, FreeAp, Functor, Monad, Nondeterminism, Scalaz, \/-, ~>}
import org.http4s._
import org.http4s.client.Client

import scalaz.concurrent.Task


/**
  * Created by hjs on 26/05/2016.
  * taken from Markus Hauck's Free Monads and Free Applicatives
  * https://vimeo.com/165928373
  */

case class Owner(id: String)
case class Repo(id: String)
case class GitHubAuth(token: String)

// this class is incomplete
case class Issue(id: Long, number: Int, title: String, body: String,
  url: String, repository_url: String,comments_url: String, events_url: String,
  user: User,
  state: String, locked: Boolean,
  assignee: Option[User],
  created_at: String,  updated_at: String, closed_at: Option[String]
)
object Issue {
  implicit def IssueCodecJson: CodecJson[Issue] =
    casecodec15(Issue.apply, Issue.unapply)(
      "id", "number", "title","body",
      "url", "repository_url", "comments_url", "events_url",
      "user",
      "state", "locked",
      "assignee",
      "created_at", "updated_at", "closed_at")

}

case class Comment(id: Long,
  url: String,  html_url: String, issue_url: String,
  body: String,
  user: User,
  created_at: String, updated_at: String
)
object Comment {
  implicit def CommentCodecJson: CodecJson[Comment] =
    casecodec8(Comment.apply, Comment.unapply)(
      "id",
      "url","html_url","issue_url",
      "body",
      "user",
      "created_at","updated_at")
}

case class User(login: String, id: Long, avatar_url: String, url: String, followers_url: String, following_url: String)

object User {
  implicit def UserCodecJson: CodecJson[User] =
    casecodec6(User.apply, User.unapply)("login", "id", "avatar_url", "url", "followers_url", "following_url")
}

// ADT

sealed trait GitHub[A]

case class ListIssues(owner: String, repo: String) extends GitHub[List[Issue]]
case class GetComments(owner: String, repo: String, issue: Int) extends GitHub[List[Comment]]
case class GetUser(name: String) extends GitHub[User]

object GitHubMonadic {


  //  val client = PooledHttp1Client()

  type GHMonadic[A] = Free[GitHub, A]

  def listIssues(owner: String, repo: String): GHMonadic[List[Issue]] =
    Free.liftF(ListIssues(owner, repo))

  def getComments(owner: String, repo: String, issue: Int): GHMonadic[List[Comment]] =
    Free.liftF(GetComments(owner, repo, issue))

  def getUser(login: String): GHMonadic[User] =
    Free.liftF(GetUser(login))


//  import cats.syntax.traverse._
//  import cats.std.list._
  import Scalaz._

  //clearly this is just a toy example and does not make much sense since the github api returns all
  //the information in each request, meaning that making seperate requests for users is not really needed
  //But this would make a lot more sense for a better more referential LinkedData API
  def allUsers(owner: String, repo: String): GHMonadic[List[(Issue, List[(Comment, User)])]] = for {
    issues <- listIssues(owner, repo)
    issueComments <- issues.traverseU((i: Issue) => getComments(owner, repo, i.number).map[(Issue, List[Comment])]((i, _)))
    users <- issueComments.traverseU { case (issue, comments) =>
      comments.traverseU((comment: Comment) =>
        getUser(comment.user.login).map((comment, _))
      ).map((issue, _))
    }
  } yield users


  implicit val mtask = new Monad[Task] {
    override def point[A](x: => A): Task[A] = Task.delay(x)
    override def bind[A, B](fa: Task[A])(f: (A) => Task[B]): Task[B] = fa.flatMap(f)
  }

}

case class GHInterpret(client: Client, auth: GitHubAuth) extends (GitHub ~> Task) {
  import org.http4s.argonaut.jsonDecoder

  def fetch(uriStr: String): Task[Json] = {
    val parsedUri = Uri.fromString(uriStr)
    parsedUri match {
      case -\/(fail) => Task.fail(new Throwable(s"bad url <$uriStr> . Error: $fail "))
      case \/-(uri) => {
        val r = Request(Method.GET, uri, HttpVersion.`HTTP/1.1`,
          Headers(Header("Authorization", s"token ${auth.token}")))
        println(s"fetch: $uri")
        client.fetchAs[Json](r)
      }
    }
  }

  override
  def apply[A](fa: GitHub[A]): Task[A] = fa match {
    case ListIssues(owner, repo) =>
      fetch(s"https://api.github.com/repos/$owner/$repo/issues").flatMap { json =>
        json.jdecode[List[Issue]].fold(
          (err, _) => Task.fail(new Throwable(err)),
          Task.now
        )
      }
    case GetComments(owner, repo, issue) =>
      fetch(s"https://api.github.com/repos/$owner/$repo/issues/$issue/comments").flatMap { json =>
        json.jdecode[List[Comment]].fold(
          (err, _) => Task.fail(new Throwable(err)),
          Task.now
        )
      }
    case GetUser(name) => fetch(s"https://api.github.com/users/$name").flatMap { json =>
      json.jdecode[User].fold(
        (msg, _) => Task.fail(new Throwable(msg)),
        Task.now
      )
    }
  }
}

object GitHubApplicative {
  type GHApplicative[A] = FreeAp[GitHub, A]
//  import cats.std.list._
//  import cats.syntax.traverse._
    import scalaz.Scalaz._

  //see: https://gist.github.com/pchiusano/8965595
  //parallel applicative
  implicit val apptask = new Applicative[Task] {
    override def point[A](x: => A): Task[A] = Task.now(x)

    override def ap[A, B](a: => Task[A])(fab: => Task[(A) => B]): Task[B] =
      Nondeterminism[Task].mapBoth(fab, a)((a1ToB, a1) => a1ToB(a1))
  }

  def listIssues(owner: String, repo: String): GHApplicative[List[Issue]] =
    FreeAp.lift(ListIssues(owner, repo))

  def getComments(owner: String, repo: String, issue: Int): GHApplicative[List[Comment]] =
    FreeAp.lift(GetComments(owner, repo, issue))

  def getUser(login: String): GHApplicative[User] =
    FreeAp.lift(GetUser(login))

  def loginsToApp(logins: List[String]): GHApplicative[List[User]] = logins.traverseU(getUser(_))

}
