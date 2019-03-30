organization := "net.andimiller"

name := "circe-diff"

version := "0.1"

scalaVersion := "2.12.8"

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core" % "0.11.1",
  "org.typelevel" %% "cats-effect" % "1.2.0",
  "co.fs2" %% "fs2-core" % "1.0.4",
  "org.tpolecat" %% "atto-core" % "0.6.5"
)


scalacOptions += "-Ypartial-unification"

enablePlugins(JmhPlugin)