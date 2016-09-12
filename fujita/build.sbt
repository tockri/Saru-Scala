name := """Saru Scala"""

version := "1.0"

scalaVersion := "2.11.7"

mainClass in Compile := Some("property.Study")

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.12.5" % "test"