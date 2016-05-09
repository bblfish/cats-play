// Comonads are objects
// http://www.haskellforall.com/2013/02/you-could-have-invented-comonads.html

import comonad.FunkyOO._

val doubleString = (s: String) => s+s
doubleString("hello")

"hello" ° doubleString

//
//The command pattern
//

//Tagged Types: http://etorreborre.blogspot.co.uk/2011/11/practical-uses-for-unboxed-tagged-types.html
type Tagged[U] = { type Tag = U }
type @@[T, U] = T with Tagged[U]

trait KelvinUnit
trait CelsiusUnit

type Kelvin = java.lang.Double @@ KelvinUnit
type Celsius = java.lang.Double @@ CelsiusUnit

def kelvin(d: java.lang.Double): Kelvin = d.asInstanceOf[Kelvin]
def celsius(d: java.lang.Double): Celsius = d.asInstanceOf[Celsius]

//case class Kelvin(temp: Double)
case class Thermostat[A] (t: Kelvin, f: Kelvin=>A)

//case class Celsius(temp: Double)

val kelvinToCelsius = (k: Kelvin) => celsius(k-273.15)

val initialThermostat = Thermostat[Celsius](kelvin(298.15), kelvinToCelsius)
def extract[A]: Thermostat[A] => A = { case Thermostat(t, f) => f(t) }

type Preview[A,B] = Thermostat[A] => B
type Adjustment[A,B] = Thermostat[A] => Thermostat[B]

extract(initialThermostat)
def up[A]: Preview[A,A] = { case Thermostat(t,f) => f(kelvin(t+1.0)) }
def down[A]: Preview[A,A] = { case Thermostat(t,f) => f(kelvin(t-1.0)) }

up(initialThermostat)
down(initialThermostat)

val string: Thermostat[Celsius] => String = {
  case Thermostat(t,f) => f(t) + " Celsius"
}

string(initialThermostat)

def up2[A]: Adjustment[A,A] = {
  case Thermostat(t,f) => Thermostat(kelvin(t+1.0),f)
}
def down2[A]: Adjustment[A,A] = {
  case Thermostat(t,f) => Thermostat(kelvin(t-1.0),f)
}

val reslt1 = initialThermostat ° (up2 andThen up2)
string(reslt1)



//now we define an extend function
def extend[A,B](preview: Preview[A,B]): Adjustment[A,B] = {
  case Thermostat(t,f) => Thermostat[B](t, x=>preview(Thermostat(x,f)))
}

val up3 = extend[Celsius,Celsius](up: Preview[Celsius,Celsius])
val resl2 = initialThermostat ° (up3 andThen up3)
string(resl2)

//show extend(up)(it) == up2(it)
{
  val it = initialThermostat
  extend[Celsius, Celsius](up)(it)
  Thermostat(it.t, x => up(Thermostat(x, it.f)))
  Thermostat(it.t, x => it.f(kelvin(it.t + 1.0)))
}
//but oops it does not match!

//prove: extract (extend preview thermostat) = preview thermostat
{
  def preview[A] = up[A]
  val therm = initialThermostat
  extract[Celsius](extend[Celsius, Celsius](preview[Celsius])(therm))
  extract[Celsius](Thermostat(therm.t, x => preview(Thermostat(x, therm.f))))
  preview(Thermostat(therm.t, therm.f))
  preview(therm)
}


//or in a more abstract manner to cover all cases
def proof[A,B](preview: Preview[A,B])(t: Thermostat[A]) = {
  val step1 = extract[B](extend[A, B](preview)(t))
  val step2 = extract[B](Thermostat(t.t,x=>preview(Thermostat(x,t.f))))
  val step3 = preview(Thermostat(t.t,t.f))
  val step4 = preview(t)
  // all the above steps are equivalent
  step4
}
//prove: extend extract thermostat = thermostat
def proof2[A](t: Thermostat[A]) = {
  val step1 = extend[A,A](extract)(t)
  val step2 = Thermostat(t.t,x=>extract(Thermostat(x,t.f)))
  val step3 = Thermostat(t.t,x=>t.f(x))
  val step4 = Thermostat(t.t,t.f)
  val step5 = t
  step5  //ie. proof2[A] == id[A]
}

// proove that if you combine an adjustment and preview, you can extend the pair of them into an adjustment, which should be identical to extending the preview alone.

def proof3[A,B,C](t1: Thermostat[A])
               (p1: Preview[A,B], p2: Preview[B,C]): Thermostat[C] = {
//val end =   extend(                   p2)(extend(p1)(t1))
  val start = extend((t: Thermostat[A])=>p2(extend(p1)(t)))(t1)
  def previewAtoC(t: Thermostat[A]): C = p2(extend(p1)(t))
  val start2 = extend(previewAtoC)(t1)
  val start3 = ((t: Thermostat[A]) => Thermostat[C](t.t, x=>previewAtoC(Thermostat(x,t.f))))(t1)
  val start4 = Thermostat[C](t1.t, x=>previewAtoC(Thermostat(x,t1.f)))
  lazy val start5 = Thermostat[C](t1.t, substart _)

  // I now need to show that x=>preview(Thermostat(x,t1.f) is the same as
  // x =>p2(Thermostat(x,x2=>p1(Thermostat(x2,t1.f))
  def substart(x: Kelvin): C = previewAtoC(Thermostat(x,t1.f))
  def substar2(x: Kelvin): C = p2(extend(p1)(Thermostat(x,t1.f)))
  def substar3(x: Kelvin): C = p2(
    ((t: Thermostat[A]) => Thermostat[B](t.t, x2=>p1(Thermostat(x2,t.f))))(Thermostat(x,t1.f))
  )
  def substar4(x: Kelvin): C = p2(
    Thermostat[B](x, x2=>p1(Thermostat(x2,t1.f)))
  )
  def subend(x: Kelvin): C = p2(Thermostat[B](x,x2=>p1(Thermostat(x2,t1.f))))
  //so they are ie substart == subend


  val end = extend(p2)(extend(p1)(t1))
  val end1 = extend(p2)(((t:Thermostat[A]) =>
    Thermostat[B](t.t, x=>p1(Thermostat(x,t.f))))(t1)
  )
  val end3 = extend(p2)(Thermostat[B](t1.t, x=>p1(Thermostat(x,t1.f))))
  val tb = Thermostat[B](t1.t, x=>p1(Thermostat(x,t1.f)))
  val end4 = ((t: Thermostat[B]) => Thermostat[C](t.t, x=>p2(Thermostat(x,t.f))))(tb)
  val end5 = Thermostat[C](tb.t, x=>p2(Thermostat(x,tb.f)))
  val end6 = Thermostat[C](t1.t, x=>p2(Thermostat(x,x2=>p1(Thermostat(x2,t1.f)))))
  lazy val end7 = Thermostat[C](t1.t, subend _)

  //and hence start5 == end7
  //and so start == end

  end
}

//find counterexample for: extend (\t' -> p (up' t')) t = extend p (up' t)

def extendUp2L[A,B](p: Preview[A,B])(t: Thermostat[A]): Thermostat[B] = {
//  val df = extend((t2: Thermostat[A]) => p(up2(t2)))(t)
//  val d2 = extend((t2: Thermostat[A]) => p(Thermostat(kelvin(t2.t+1.0),t2.f)))(t)
//  val preview = (t2: Thermostat[A]) => p(Thermostat(kelvin(t2.t+1.0),t2.f))
//  val d3 = extend(preview)(t)
//  val d4 = ((t: Thermostat[A]) => Thermostat[B](t.t, x=>preview(Thermostat(x,t.f))))(t)
//  val d5 = Thermostat[B](t.t, x=>preview(Thermostat(x,t.f)))
  val d6 = Thermostat[B](t.t, x=>p(Thermostat(kelvin(x+1.0),t.f)))
  d6
}

def extendUp2R[A,B](p: Preview[A,B])(t: Thermostat[A]): Thermostat[B] = {
//  val df = extend(p)(up2(t))
//  val d2 = extend(p)(Thermostat(kelvin(t.t+1.0),t.f))
//  val d3 = ((t: Thermostat[A]) => Thermostat[B](t.t, x=>p(Thermostat(x,t.f))))(Thermostat(kelvin(t.t+1.0),t.f))
  val d4 = Thermostat[B](kelvin(t.t+1.0), x=>p(Thermostat(x,t.f)))
  d4
}

extendUp2L[Celsius,Celsius](up)(initialThermostat) ° extract
extendUp2R[Celsius,Celsius](up)(initialThermostat) ° extract
//those are the same

def compose[A,B,C](p1: Preview[A,B], p2: Preview[B,C]): Preview[A,C] = {
  (t: Thermostat[A])=>p2(extend(p1)(t))
}

val celsiusToKelvin = (c: Celsius) => kelvin(c+273.15)

def double[A]: Preview[Kelvin,Kelvin] = (a: Thermostat[Kelvin]) => kelvin((a.f andThen a.f)(a.t))
def toKelvin: Preview[Celsius,Kelvin] = (a: Thermostat[Celsius]) => celsiusToKelvin(a.f(a.t))

def zeroPreview[A]: Preview[A,Celsius] = (t: Thermostat[A])=>celsius(0.0)
def show[A] = (t: Thermostat[A]) => t.t

val inialThermAsK = extend(toKelvin)(initialThermostat)
def x3[A]: Preview[Kelvin,Kelvin] = (a: Thermostat[Kelvin]) => kelvin(a.t*3)


extendUp2L[Celsius,Celsius](extract)(initialThermostat) ° show
extendUp2R[Celsius,Celsius](extract)(initialThermostat) ° show
extend[Celsius,Celsius](extract)(initialThermostat) ° show
extend[Celsius,Celsius](zeroPreview)(initialThermostat) ° show



//so the answer to the exercise is that the show function that
//accesses the temperature directly reveals the difference between the
//two implementations!
// If one removes the accessor (ie, hide that t.t field), then one just ends
// up with maps of the extracted category.
// The difference then between working with Thermostat and working on
// the extracted categories then is that with Thermostat one always keeps
// access to a read only view of the underlying data point. So one can always
// find out how the calculation was built up.

