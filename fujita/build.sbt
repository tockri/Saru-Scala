name := """Saru Scala"""

version := "1.0"

scalaVersion := "2.12.1"

mainClass in Compile := Some("monad.Study")

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.4" % "test"