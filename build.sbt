ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.4.0"

lazy val root = (project in file("."))
  .settings(
    name := "scala-search-engine"
  )

libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4"