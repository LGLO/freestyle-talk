scalacOptions := Seq("-deprecation", "-feature", "-language:higherKinds", "-language:implicitConversions")

organization := "io.scalac"
scalaVersion := "2.12.4"
version := "1.0.0-SNAPSHOT"
name := "freestyle-talk"

lazy val root = (project in file("."))
  .settings(libraryDependencies += "io.frees" %% "frees-core" % "0.6.2")
  .settings(addCompilerPlugin("org.scalameta" %% "paradise" % "3.0.0-M10" cross CrossVersion.full))
