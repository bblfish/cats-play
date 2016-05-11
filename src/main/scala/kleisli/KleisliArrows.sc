// examples from http://www.casualmiracles.com/2012/07/02/a-small-example-of-kleisli-arrows/


import java.io.File

import cats.data.Kleisli
import cats.std.option._
import cats.syntax.all._

import scala.io.Source

val files: String => List[File] =
   (dir) => new File(dir).listFiles().toList

//"hello world  how are you?".split("\\W+")
val lines: File => List[String] =
   (f) => Source.fromFile(f).getLines().toList

val lineLength = lines >>=  (l: String => List(l.length))

//val lengths: File => List[Int] =
//  (f) => Source.fromFile(f).getLines().flatMap(_.split("\\W+")).toList.size

//(l => l.length())
//☆