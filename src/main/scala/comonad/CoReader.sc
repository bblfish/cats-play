import comonad.FunkyOO._
import cats._
import cats.data.Cokleisli
import cats.implicits._

// reworking of CoMonads are objects but now using cats
// http://www.haskellforall.com/2013/02/you-could-have-invented-comonads.html

type Tagged[U] = { type Tag = U }
type @@[T, U] = T with Tagged[U]

trait KelvinUnit
trait CelsiusUnit

type Kelvin = java.lang.Double @@ KelvinUnit
type Celsius = java.lang.Double @@ CelsiusUnit

def kelvin(d: java.lang.Double): Kelvin = d.asInstanceOf[Kelvin]
def celsius(d: java.lang.Double): Celsius = d.asInstanceOf[Celsius]

//case class Kelvin(temp: Double)
case class Thermostat[A] (t: Kelvin, f: Kelvin=>A) {
  //this is the OO version
  def >=>[B](show: Thermostat[A]=>B) = Thermostat[B](t,x=>show(Thermostat(x,f)))
}

object X {
   implicit val coreader = new Comonad[Thermostat] {
     override def extract[A](t: Thermostat[A]): A = t.f(t.t)

     override def coflatMap[A, B](ta: Thermostat[A])(f: (Thermostat[A]) => B): Thermostat[B] = Thermostat[B](ta.t,(t: Kelvin)=>f(Thermostat(t,ta.f)))

     override def map[A, B](ta: Thermostat[A])(f: (A) => B): Thermostat[B] = Thermostat(ta.t,ta.f andThen f)
   }


}

import X._

type Preview[A,B] = Thermostat[A] => B
type Adjustment[A,B] = Thermostat[A] => Thermostat[B]

val kelvinToCelsius = (k: Kelvin) => celsius(k-273.15)
def up: Preview[Kelvin,Kelvin] = { case Thermostat(t,f) => f(kelvin(t+1.0)) }
def down: Preview[Kelvin,Kelvin] = { case Thermostat(t,f) => f(kelvin(t-1.0)) }

val it = Thermostat[Kelvin](kelvin(3.0), identity)

val t0 =  it.extract
val t1 =  it.coflatMap(up).extract
val t1o = it >=> (up) extract
val t2 =  it.coflatMap(up).coflatMap(up).extract
val t2o = it >=> (up) >=> (up) extract

//test counterargument from http://gelisam.blogspot.co.uk/2013/07/comonads-are-neighbourhoods-not-objects.html

val square: Preview[Kelvin,Kelvin] = { case Thermostat(t,f) => f(kelvin(t*t)) }

val s1 = it.coflatMap(up).coflatMap(square).extract
val s2 = it.coflatMap(up).coflatMap(up).coflatMap(square).extract
val s3 = it >=> up >=> up >=> square >=> up  extract

//-- testing with CoReader
type CoReader[A,B] = Cokleisli[Thermostat,A,B]
def CoReader[A,B](f: Thermostat[A]=>B): Cokleisli[Thermostat,A,B]= Cokleisli[Thermostat,A,B](f)

val kup = CoReader[Kelvin,Kelvin](up)
val kdown = CoReader[Kelvin,Kelvin](down)
val ksquare = CoReader[Kelvin,Kelvin](square)

val k2 = (kup andThen kup andThen ksquare).run(it)
val k3 = (kup andThen kup andThen ksquare andThen kup).run(it)

//one can use the following implicit to avoid wrapping each of the Previews in a cokleisli
implicit
def toCoReader(preview: Preview[Kelvin,Kelvin]):Cokleisli[Thermostat,Kelvin,Kelvin]=CoReader[Kelvin,Kelvin](preview)

val k4 = (CoReader[Kelvin,Kelvin](up) andThen up andThen square andThen up).run(it)

