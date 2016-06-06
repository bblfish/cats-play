package free

import _root_.argonaut.{CodecJson, _}
import _root_.argonaut.Argonaut._

import scalaz.{-\/, Applicative, Coproduct, Free, FreeAp, Functor, Monad, NaturalTransformation, Nondeterminism, Scalaz, Tag, \/-, ~>}
import org.http4s._
import org.http4s.client.Client

import Scalaz._
import scalaz.Tags.Parallel
import scalaz.concurrent.Task
import scalaz.concurrent.Task.ParallelTask


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
        client.fetchAs[Json](r).onFinish(opt=>Task.now(println(s"finished <$uri> with $opt")))
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

object ParallelTaskNat extends (Task ~> ParallelTask) {
  override def apply[A](fa: Task[A]): ParallelTask[A] = Parallel(fa)
}

object TaskFork extends (Task ~> Task) {
  override def apply[A](fa: Task[A]): Task[A] = Task.fork(fa)
}


object GitHubApplicative {
  type GHApplicative[A] = FreeAp[GitHub, A]

  import scalaz.Scalaz._

  def listIssues(owner: String, repo: String): GHApplicative[List[Issue]] =
    FreeAp.lift(ListIssues(owner, repo))

  def getComments(owner: String, repo: String, issue: Int): GHApplicative[List[Comment]] =
    FreeAp.lift(GetComments(owner, repo, issue))

  def getUser(login: String): GHApplicative[User] =
    FreeAp.lift(GetUser(login))

  def loginsToApp(logins: List[String]): GHApplicative[List[User]] = logins.traverseU(getUser(_))

  //
  //all functions below to optimise fetching logins
  //
  val logins: GitHub ~> λ[α => Set[String]] = new (GitHub ~> λ[α => Set[String]]) {
    override def apply[A](fa: GitHub[A]): Set[String] = fa match {
      case GetUser(name) => Set(name)
      case _ => Set()
    }
  }

  def extractLogins(p: GHApplicative[_]): Set[String] = p.analyze(logins)

  def precompute[A, F[_] : Applicative](
    p: GHApplicative[A],
    interp: GitHub ~> F
  ): F[Map[String, User]] = {
    val userLogins = extractLogins(p).toList
    val fetched: F[List[User]] = userLogins.traverseU(getUser).foldMap(interp)
    Functor[F].map(fetched)(userLogins.zip(_).toMap)
  }

  def optimizeNat[F[_] : Applicative](
    nameToUser: Map[String, User],
    interp: GitHub ~> F
  ): GitHub ~> F = new (GitHub ~> F) {
    override def apply[A](fa: GitHub[A]): F[A] = fa match {
      case ffa@GetUser(login) => nameToUser.get(login) match {
        case Some(user) => Applicative[F].pure(user)
        case None => interp(ffa)
      }
      case other => interp(other)
    }
  }

  //optimising interpreter from GHApplicative to Task
  case class InterpretOptNat[A](
    //we can't just be general and have an T: Applicative as otherwise
    //we don't get the flatMap below!
    interp: GitHub ~> Task
  ) extends (GHApplicative ~> Task) {

    import scalaz.Tags._
    import scalaz.concurrent.Task.ParallelTask
    import scalaz.syntax.tag._
    import Task.taskParallelApplicativeInstance


    override
    def apply[A](p: GHApplicative[A]): Task[A] = {
      val mapping: ParallelTask[Map[String, User]] =
        precompute(p, interp andThen ParallelTaskNat)
      // we need to be able to move from a ParallelTask to a Task to get the
      // flatMap
      mapping.unwrap.flatMap { m =>
        val betterNat = optimizeNat(m, interp andThen ParallelTaskNat)
        p.foldMap(betterNat).unwrap //again we want to return a normal Task
      }
    }
  }
}



object GitHubBoth {
  import GitHubApplicative.GHApplicative
  type GHCo[A] = Coproduct[GitHub,GHApplicative,A]
  type GHBoth[A] = Free[GHCo[?],A]

  implicit class ScalaZNatTrans[F[_],G[_]](val nt: NaturalTransformation[F,G]) extends AnyVal {

    def or[H[_]](h:  NaturalTransformation[H,G]): NaturalTransformation[Coproduct[F, H, ?],G] =
      new (NaturalTransformation[Coproduct[F, H, ?],G]) {
        def apply[A](fa: Coproduct[F, H, A]): G[A] = fa.run match {
          case -\/(ff) => nt.apply(ff)
          case \/-(gg) => h.apply(gg)
        }
      }
  }

  def listIssuesB(owner: String, repo: String): GHBoth[List[Issue]] =
    Free.liftF(Coproduct.left(ListIssues(owner, repo)))

  def getCommentsB(owner: String, repo: String, issue: Int): GHBoth[List[Comment]] =
    Free.liftF(Coproduct.left(GetComments(owner, repo, issue)))

  def getUserB(login: String): GHBoth[User] =
    Free.liftF(Coproduct.left(GetUser(login)))

  def embed[A](g: GHApplicative[A]):  GHBoth[A] =
    Free.liftF[Coproduct[GitHub,GHApplicative,?],A](Coproduct.right(g))

  def interpretMix(client: Client, auth: GitHubAuth): GHCo ~> Task = {
    import GitHubApplicative.InterpretOptNat
    val interp = GHInterpret(client, auth)
    val appInterp = InterpretOptNat(interp)
    interp.or[GHApplicative](appInterp)
  }

  def allUsersM(owner: String, repo: String): GHBoth[List[(Issue, List[(Comment, User)])]] = for {
    issues <- listIssuesB(owner, repo)
    issueComments <-  embed {
      issues.traverseU((i: Issue) =>
        GitHubApplicative.getComments(owner, repo, i.number).map[(Issue, List[Comment])]((i, _)))
    }
    users <- embed {
      issueComments.traverseU { case (issue, comments) =>
        comments.traverseU((comment: Comment) =>
          GitHubApplicative.getUser(comment.user.login).map((comment, _))
        ).map((issue, _))
      }
    }
  } yield users

}
