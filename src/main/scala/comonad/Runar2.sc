import cats._
import cats.implicits._
import cats.data.{NonEmptyList, OneAnd}
import cats.data.StateT

//
// http://blog.higher-order.com/blog/2015/10/04/scala-comonad-tutorial-part-2/


val l = List(List(1,2,3),List(1,2),List(1))
l.map(l=>l.fold(0)(_+_))

val nel4 = NonEmptyList(1,2,3,4)
val res = nel4.coflatMap(_.foldLeft(0)(_+_))

case class Cofree[F[_],A](counit: A, sub: F[Cofree[F,A]]) {
  def duplicate(implicit F: Functor[F]): Cofree[F,Cofree[F,A]] = {
    val x = F.map(sub)(_.duplicate)
    Cofree(this, x)
  }
  def map[B](f: A=>B)(implicit F: Functor[F]): Cofree[F,B] =
    Cofree[F,B](f(counit),F.map(sub)(_.map(f)))

  def extend[B](f: Cofree[F,A]=>B)(implicit F: Functor[F]): Cofree[F,B] = {
    val d = duplicate
    d.map(f)
  }
}

type Tree[A]=Cofree[List,A]
object Tree {
  def apply[A](tip: A, sub: List[Tree[A]] = Nil) = Cofree(tip, sub)
  def unapply[A](arg: Tree[A]): Option[(A,List[Cofree[List,A]])] = Some(arg.counit,arg.sub)
}

val t1 = Tree[Int](1, List(
  Tree(2, List(Tree(3))),
  Tree(4, List(Tree(5)))))

def sum(t: Tree[Int]): Int = t match {
  case Tree(tip,sub) => tip + {
    val x= sub.map(sum(_))
    x.foldRight(0)(_+_)
  }
}

t1.extend(sum)


trait Adjunction[F[_],G[_]] {
  def left[A,B](f: F[A] => B): A => G[B]
  def right[A,B](f: A => G[B]): F[A] => B
}

def homSetAdj[R] = new Adjunction[(?, R), R => ?] {
  def left[A,B](f: ((A, R)) => B): A => R => B =
    Function.untupled(f).curried
  def right[A,B](f: A => R => B): ((A, R)) => B =
    Function.uncurried(f).tupled
}

def monad[F[_],G[_]](A: Adjunction[F,G])(implicit G: Functor[G]) =
  new Monad[λ[α => G[F[α]]]] {
    override
    def pure[A](a: A): G[F[A]] =
      A.left[A,F[A]](identity[F[A]])(a)  //A.left[A,F[A]](...): A => G[F[A]]
    override
    def flatMap[A,B](a: G[F[A]])(fa: A => G[F[B]]): G[F[B]] = {
      val upf: F[A] => F[B] = A.right[A,F[B]](fa)
      G.map[F[A], F[B]](a)(upf)
    }
  }

def comonad[F[_],G[_]](A: Adjunction[F,G])(implicit F: Functor[F], G: Functor[G]) =
  new Comonad[λ[α => F[G[α]]]] {
    def extract[A](a: F[G[A]]): A =
      A.right[G[A],A](identity[G[A]])(a) //A.right[G[A],A](...): F[G[A]] => A
    def coflatMap[A,B](a: F[G[A]])(f: F[G[A]] => B): F[G[B]] = {
      val downf: G[A] => G[B] = A.left[G[A],B](f)
      F.map(a)(downf)
    }
    override
    def map[A, B](fa: F[G[A]])(f: (A) => B): F[G[B]] = F.map(fa){ ga: G[A] =>
       G.map(ga)(f)
    }
  }

//homSetAdj[R] => Adjunction[[α](α, R),[β]R => β]
def M[S] = monad[λ[α => (α, S)],λ[β => S => β]](homSetAdj[S])

val state = M[Int].pure("hello")
state(2)

implicit def idstate[S] = new Functor[λ[α => (α, S)]] {
  override def map[A, B](fa: (A, S))(f: (A) => B): (B, S) = (f(fa._1),fa._2)
}

type ST[S,A] = (S => A, S)
implicit def storeComonad[S]: cats.Comonad[λ[α=>(S => α, S)]] = comonad[λ[α => (α, S)],λ[β => S => β]](homSetAdj[S])

type Grid = List[List[Boolean]]
type Position = (Int, Int)   // (linenbr, colnbr)

//8x8 grid
val lifegrid: Grid = List(
  List(false, false, true, false, false, false, false,false),
  List(false, true, false, false, false, false, false,false),
  List(false, false, true, false, false, false, false,false),
  List(true, false, false, true, false, false, false,false),
  List(false, true, false, false, false, false, false,false),
  List(false, false, false, false, false, false, false,false),
  List(false, false, false, false, false, false, false,false),
  List(false, false, false, false, false, false, false,false)
)
def printLifeGrid(grid: Grid)  = {
  val strL = grid.map(row => row.map(if (_) "*" else "-").mkString)
  "\n"+strL.mkString("\n")
}
printLifeGrid(lifegrid)

def find(pos: Position): Boolean = pos match {
  case (line,row) => lifegrid(line)(row)
}

val x00 = find((0,0))
val x02 = find((0,2))
val x40 = find((4,0))
val x30 = find((3,0))
val x22 = find((2,2))
val x77 = find((7,7))

type PointedGrid = ST[Position,Boolean]
val store: PointedGrid  = (find _, (0,0))
val r11 = store.extract

def nextGen(st: PointedGrid): Boolean = {
  def translateBy(x: Int, y: Int): (Int,Int) = {
    val pos = st._2
    def wrap(x: Int)= if (x>0)x else 8+x
    (wrap(pos._1+x)%8,wrap(pos._2+y)%8)
  }
  val isalive = st._1
  val positionsAroundMe = List((-1, -1), (-1, 0), (-1, 1),
    (0, -1), (0, 1),
    (1, -1), (1, 0), (1, 1))

  val next = positionsAroundMe.map(p=>isalive(translateBy(p._1,p._2)))
  val friends = next.filter(x=>x).size
  if (st.extract) { //am alive
     friends == 2 || friends == 3
  } else { // am dead
     friends == 2
  }
}

val next: PointedGrid = store.coflatMap(nextGen _)
val next2: PointedGrid = next.coflatMap(nextGen _)



def toList(pointedGrid: PointedGrid) =
  (0 to 7).toList.map { row =>
    (0 to 7).toList.map(col => pointedGrid._1((row, col)))
  }

lifegrid
printLifeGrid(toList(store))
printLifeGrid(toList(next ))
printLifeGrid(toList(next2))

val in1000: PointedGrid = (1 to 1000).foldRight(store)((_,s)=> s.coflatMap(nextGen _))
//stack overflow:
//printLifeGrid(toList(in1000))