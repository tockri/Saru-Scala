name := """Saru Scala"""

version := "1.0"

scalaVersion := "2.12.1"

mainClass in Compile := Some("monad.Study")

libraryDependencies ++= Seq(
  "org.scalacheck" %% "scalacheck" % "1.13.4" % "test",
  "org.scalaz" %% "scalaz-core" % "7.2.8",
  "org.scala-lang" % "scala-reflect" % "2.12.1"
)