import cats.data.Kleisli
import cats.std.option._
import cats.syntax.all._

// simple example taken from  http://typelevel.org/cats/tut/kleisli.html

val parse = (s: String) => try { Some(s.toInt) } catch { case _: NumberFormatException => None }
val reciprocal = (i: Int) => if (i == 0) None else Some(1.0 / i)

val parseK = Kleisli[Option,String,Int](parse)
val reciprocalK = Kleisli[Option,Int,Double](reciprocal)

val re = (x: String) =>  Option(x) >>= parse >>= reciprocal
re("2")

val x =(parseK andThen reciprocal).run("2")


//
// config example taken from http://typelevel.org/cats/tut/kleisli.html
//

case class DbConfig(url: String, user: String, pass: String)
case class Db(s: String)
object Db {
  val fromDbConfig= Kleisli[Option, DbConfig, Db](x=>Some(Db(x.url+" for "+x.user)))
}

case class ServiceConfig(addr: String, port: Int)
case class Service(s: String)
object Service {
  val fromServiceConfig = Kleisli[Option, ServiceConfig, Service](x=>Some(Service(x.addr+":"+x.port)))
}

case class AppConfig(dbConfig: DbConfig, serviceConfig: ServiceConfig)
case class App(db: Db, service: Service)
def appFromAppConfig: Kleisli[Option, AppConfig, App] =
  for {
    db <- Db.fromDbConfig.local[AppConfig](_.dbConfig)
    sv <- Service.fromServiceConfig.local[AppConfig](_.serviceConfig)
  } yield new App(db, sv)

val app = appFromAppConfig.run(
  AppConfig(
    DbConfig("http://h.i/","H","XXX"),
    ServiceConfig("localhost",8080)
  ))

