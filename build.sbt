val scala3Version = "3.1.1"

val catsVersion = "2.7.0"
val catsEffectVersion = "3.3.5"
val scalaTestVersion = "3.2.11"
val fs2Version = "3.2.4"

lazy val root = project
  .in(file("."))
  .settings(
    fork := true, // Fork to separate process
    run / connectInput := true, // Connects stdin to sbt during forked runs
    outputStrategy := Some(StdoutOutput), // Get rid of output prefix
    name := "Blockchain",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % catsVersion,
      "org.typelevel" %% "cats-effect" % catsEffectVersion,
      "co.fs2" %% "fs2-core" % fs2Version,
      "co.fs2" %% "fs2-io" % fs2Version,
      "org.scalatest" %% "scalatest" % scalaTestVersion % Test
    ),
    scalacOptions ++= Seq(
      "-deprecation", // emit warning and location for usages of deprecated APIs
      "-explain", // explain errors in more detail
      "-explain-types", // explain type errors in more detail
      "-feature", // emit warning and location for usages of features that should be imported explicitly
      "-indent", // allow significant indentation.
      "-new-syntax", // require `then` and `do` in control expressions.
      "-print-lines", // show source code line numbers.
      "-unchecked", // enable additional warnings where generated code depends on assumptions
      "-Ykind-projector", // allow `*` as wildcard to be compatible with kind projector
      "-Xfatal-warnings", // fail the compilation if there are any warnings
      "-Xmigration" // warn about constructs whose behavior may have changed since version
    )
  )
