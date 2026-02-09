lazy val root = (project in file("."))
  .settings(
    inThisBuild(List(
      organization := "chess",
      scalaVersion := "3.8.1"
    )),
    name := "stair-chess"
  )
scalacOptions ++= Seq("-deprecation", "-feature", "-language:implicitConversions")
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % Test

Test / logBuffered := false