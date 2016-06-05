import sbt._
import sbt.Keys._

object ApplicationBuild extends Build {

  val catsVersion = "0.6.0"

  val catsAll = "org.typelevel" %% "cats" % catsVersion

  val macroParadise = compilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

  val kindProjector = compilerPlugin("org.spire-math" %% "kind-projector" % "0.7.1")
  val resetAllAttrs = "org.scalamacros" %% "resetallattrs" % "1.0.0-M1"

  val specs2Version = "3.6"
  // use the version used by discipline
  val specs2Core = "org.specs2" %% "specs2-core" % specs2Version
  val specs2Scalacheck = "org.specs2" %% "specs2-scalacheck" % specs2Version
  val scalacheck = "org.scalacheck" %% "scalacheck" % "1.12.4"

  //for eff-cats
  val effcats = "org.atnos" %% "eff-cats" % "1.6.2"
  val si27212fix = compilerPlugin("com.milessabin" % "si2712fix-plugin_2.11.8" % "1.2.0")

  val typesafeconfig =  "com.typesafe" % "config" % "1.3.0"

  //in order to run from `sbt test:console`, to reduce the requests to github in Free examples
  val ammonite = "com.lihaoyi" % "ammonite-repl" % "0.5.8" % "test" cross CrossVersion.full

  lazy val http4sVersion = "0.14.0a-SNAPSHOT"

  val http4s = Seq(
    "org.http4s" %% "http4s-dsl" % http4sVersion,
    "org.http4s" %% "http4s-argonaut" % http4sVersion,
//    "org.http4s" %% "http4s-blaze-server" % http4sVersion,
    "org.http4s" %% "http4s-blaze-client" % http4sVersion,
    "io.argonaut" %% "argonaut" % "6.2-M1"
  )


  lazy val root = (project in file(".")).
    settings(
      organization := "net.bblfish",
      name := "cats-play",
      scalaVersion := "2.11.8",
      resolvers += Resolver.sonatypeRepo("snapshots"), //for http4s
      libraryDependencies ++= Seq(
        catsAll, effcats, ammonite,
        specs2Core % Test, specs2Scalacheck % Test, scalacheck % Test,
        macroParadise, kindProjector, resetAllAttrs, si27212fix
      ) ++ http4s,
      initialCommands in (Test, console) := """ammonite.repl.Main.run("")""",
      scalacOptions ++= Seq(
        "-deprecation",
        "-encoding", "UTF-8",
        "-feature",
        "-language:_"
      )
    )
}