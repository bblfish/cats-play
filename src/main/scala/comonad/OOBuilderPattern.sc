// Comonads are objects
// http://www.haskellforall.com/2013/02/you-could-have-invented-comonads.html

// The Builder Pattern chapter

import comonad.FunkyOO._

case class Config(private val options: Opts)

type Opt = String
type Opts = List[Opt]
type Builder = Opts => Config
type Setter = Builder => Config
type Appender = Builder => Builder


val r = Config(List("hello"))

val default: Builder = (opts: Opts) => Config("-Wall"::opts)

val profile: Builder => Config =
  (b: Builder) =>  b(List("-prof","-auto-all"))

profile(default)

val goFaster: Builder => Config =
  (b: Builder) => b(List("-O2"))

def profile2: Builder => Builder =
  (builder: Builder) => (opts: Opts) => builder(List("-prof","-auto-all")++opts)

def goFaster2: Builder => Builder =
  (builder: Builder) => (opts: Opts) => builder(List("-O2")++opts)

def extract: Setter = (builder: Builder) => builder(List())

profile2(default)(List("-user=Henry"))
profile2(default) ° extract
val oo1 = default ° profile2  ° goFaster2 ° extract

def extend: Setter => Appender =
  (setter: Setter) => (builder: Builder) => {
    (opts2: Opts) => setter( (opts: Opts) => builder(opts ++ opts2))
}

val oo2 = default ° extend(goFaster) ° extend(profile) ° extract

//proove: extract (extend setter builder) = setter builder
def proof1(setter: Setter)(builder: Builder): Config = {
  val s1 = extract(extend(setter)(builder))
  val s2 = extract((opts2: Opts) => setter((opts: Opts) => builder(opts ++ opts2)))
  val s3 = ((opts2: Opts) => setter((opts: Opts) => builder(opts ++ opts2)))(List())
  val s4 = setter((opts: Opts) => builder(opts ++ List()))
  val s5 = setter((opts: Opts) => builder(opts))
  val s6 = setter(builder)
  s6
}

//proove: extend extract builder = builder
def proof2(builder: Builder): Builder = {
  val s1 = extend(extract)(builder)
  val s2 = (opts2: Opts) => extract( (opts: Opts) => builder(opts ++ opts2))
  val s3 = (opts2: Opts) => builder(List() ++ opts2)
  val s4 = builder
  s4
}

//    extend (\b' -> s2 (extend s1 b')) b
//  = extend (       s2)(extend s1 b)

def proof3(bldr: Builder)(s1: Setter, s2: Setter): Builder = {
  def setterFnct(b: Builder): Config = s2(extend(s1)(b))

  val p1 = extend(setterFnct)(bldr)
  val p2 = ((builder: Builder) => {
    (opts2: Opts) => setterFnct( (opts: Opts) => builder(opts ++ opts2))
  } )(bldr)
  val p3 = (opts2: Opts) => setterFnct( (opts: Opts) => bldr(opts ++ opts2))
  def buildrFunc(opts2: Opts) =  (opts: Opts) => bldr(opts ++ opts2)
  val p5 = (opts2: Opts) => setterFnct(buildrFunc(opts2))
  val p6 = (opts2: Opts) => s2(extend(s1)(buildrFunc(opts2)))
  val p7 = (opts2: Opts) => s2((opts: Opts) => s1( buildrFunc(opts ++ opts2)))
  val p8 = (opts2: Opts) => s2((opts: Opts) => s1( (opts3: Opts) =>bldr(opts3++opts ++ opts2)))


  val r0 = extend(s2)(extend(s1)(bldr))
  val r1 = ((builder: Builder) => {
    (opts2: Opts) => s2((opts: Opts) => builder(opts ++ opts2))
  })(extend(s1)(bldr))
  val r2 = (opts2: Opts) => s2((opts: Opts) => extend(s1)(bldr)(opts ++ opts2))
  val r3 = (opts2: Opts) => s2((opts: Opts) => extend(s1)(bldr)(opts ++ opts2))
  val r4 = (opts2: Opts) => s2((opts: Opts) => s1( (opts3: Opts) => bldr(opts3 ++opts++opts2)))

  // p8 == r4 and so r0 == p1 which is what we wanted to prove
  // but there must be a better way to proove this

  r0
}