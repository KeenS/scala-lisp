name := "hello"

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
  "org.scala-lang" % "scala-reflect" % scalaVersion.value)


// Compilation
scalacOptions ++= Seq(
  "-unchecked",
  "-deprecation",
  "-feature"
)
