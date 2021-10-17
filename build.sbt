name := "expresso"

version := "0.3.0-SNAPSHOT"

scalaVersion := "2.13.3"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.1.0" % "test",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2",
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.0"
)

libraryDependencies += "com.regblanc" %% "scala-smtlib" % "0.2.1-41-gc3f60dd"

scalacOptions ++= Seq(
  "-encoding",
  "utf8",
  "-Xfatal-warnings",
  "-deprecation",
  "-feature",
  "-unchecked",
  // "-Xlint",
  // "Wdead-code",
  "-language:implicitConversions"
)

Compile / console / scalacOptions --= Seq(
  "-Xlint",
  "-Xfatal-warnings"
)

run / fork := true
Test / fork := true
Global / cancelable := true
assembly / test := {}
run / javaOptions += "-Xss8m"
