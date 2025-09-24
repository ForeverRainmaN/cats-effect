ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.7.3"

lazy val root = (project in file("."))
  .settings(
    name := "cats-effect",

    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect" % "3.6.3"
    ),

    scalacOptions ++= Seq(
      "-deprecation",
      "-encoding", "utf-8",
      "-Wunused:all",
      "-Wnonunit-statement"
    )
  )