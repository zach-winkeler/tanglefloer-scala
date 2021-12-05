ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.7"

lazy val root = (project in file("."))
  .settings(
    name := "tanglefloer-scala"
  )

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.3.5"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.10"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.10" % "test"
libraryDependencies += "org.scalatestplus" %% "scalacheck-1-15" % "3.2.10.0" % "test"