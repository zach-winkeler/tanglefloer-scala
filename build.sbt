ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.14"

lazy val root = (project in file("."))
  .settings(
    name := "tanglefloer-scala"
  )

libraryDependencies += "org.typelevel" %% "cats-core" % "2.7.0"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.10"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.10" % "test"
libraryDependencies += "org.scalatestplus" %% "scalacheck-1-15" % "3.2.10.0" % "test"

libraryDependencies += "org.scala-graph" %% "graph-core" % "1.13.4"
libraryDependencies += "org.scala-graph" %% "graph-dot" % "1.13.3"