val scala3Version = "3.1.3"

lazy val root = project
  .in(file("."))
  .settings(
    name := "tinka-scala",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.1",
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test,
    scalacOptions ++= Seq(
      "-deprecation",
      "-explain",
      "-explain-types",
      "-feature",
      "-indent",
      "-new-syntax",
      "-print-lines",
      "-unchecked",
      "-Ykind-projector",
      "-Xfatal-warnings",
      "-Xmigration"
    )
  )
