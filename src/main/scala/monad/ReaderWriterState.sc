// Article http://underscore.io/blog/posts/2014/07/27/readerwriterstate.html

import cats.data.{Kleisli, Reader}
import cats.std.option._
import cats.syntax.all._

import scala.util.control.NonFatal

type Key = String

trait Transaction

/* Work represents a unit of work to do against the Database
 * It is a type alias for a scalaz.Reader, which wraps
 * a Transaction => A
 */

type Work[A] = Reader[Transaction, A]

object Database {

  object MyTransaction extends Transaction

  // Run now requires Work
  def run[T](work: Work[T]): T =
    try {
      startTransaction()
      val result = work.run(MyTransaction)
      commit()
      result
    } catch {
      case NonFatal(whatever) => rollback(); throw whatever
    }

  def startTransaction() = {}
  def commit() = {}
  def rollback() = {}

  // lift operations into Work - note both of these do nothing here
  def put[A](key: Key, a: A): Work[Unit] =
    Reader(Transaction => {})

  def find[A](key: Key): Work[Option[A]] =
    Reader(Transaction => None)

  def find2[A](key: Key): Transaction => Option[A] = {
    t: Transaction => None
  }
}

// the program
val work: Work[Option[String]] =
  for {
    _ <- Database.put("foo", "Bar")
    found <- Database.find[String]("foo")
  } yield found

val w2 = Database.put("foo", "Bar").flatMap(_ =>  Database.find[String]("foo"))


// now run the program
val result: Option[String] = Database.run(work)
val result2 = Database.run(w2)