ThisBuild / organization := "com.github.nhweston"
ThisBuild / scalaVersion := "3.1.2"
ThisBuild / scalacOptions :=
  Seq(
    "-deprecation",
    "-encoding", "UTF-8",
    "-feature",
    "-language:_",
    "-unchecked",
  )

lazy val dependencies =
  Seq()

lazy val root =
  (project in file("."))
    .settings(
      name := "scala-pg",
      libraryDependencies ++= dependencies,
      Compile / scalaSource := baseDirectory.value / "src",
    )
