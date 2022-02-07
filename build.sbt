val scala3Version = "3.1.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "Blockchain",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "com.novocode" % "junit-interface" % "0.11" % "test",
      "org.typelevel" %% "cats-core" % "2.7.0",
      "org.typelevel" %% "cats-effect" % "3.3.5"
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
