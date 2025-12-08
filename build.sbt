ThisBuild / version := "1.0"
ThisBuild / scalaVersion := "3.3.6"

lazy val root = (project in file(".")).settings(name := "archi_query")

libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "2.4.0"
libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.4.0-M15"
