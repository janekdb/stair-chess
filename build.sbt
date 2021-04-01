lazy val root = (project in file("."))
  .settings(
    inThisBuild(List(
      organization := "chess",
      scalaVersion := "2.13.5"
    )),
    name := "stair-chess"
  )

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.5" % Test