import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.example",
      scalaVersion := "2.12.7",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "fp-in-scala",
    libraryDependencies ++= Seq(
      "org.scalacheck" %% "scalacheck" % "1.14.0" % Test,
      scalaTest % Test
    )
  )
