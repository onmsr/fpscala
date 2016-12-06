import sbt._
import sbt.Keys._

object FpscalaBuild extends Build {

  lazy val fpscala = Project(
    id = "fpscala",
    base = file("."),
    settings = Project.defaultSettings ++ Seq(
      name := "fpscala",
      organization := "onmsr.fpscala",
      version := "0.1-SNAPSHOT",
      scalaVersion := "2.11.5",
      libraryDependencies ++= Seq(
        "org.scala-lang" % "scala-compiler" % "2.11.5",
        "org.specs2" %% "specs2-core" % "3.8.5" % "test",
        "org.mockito" % "mockito-core" % "2.2.28" % "test"
      ),
      scalacOptions in Test ++= Seq("-Yrangepos")
    )
  )
}
