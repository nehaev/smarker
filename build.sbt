
val scala3Version = "3.3.7"

lazy val root = project
    .in(file("."))
    .settings(
        name := "smarker",
        version := "0.1.0-SNAPSHOT",

        scalaVersion := scala3Version,

        scalacOptions ++= Seq(
            "-deprecation", // Emit warning and location for usages of deprecated APIs.
            "-explaintypes", // Explain type errors in more detail.
            "-feature", // Emit warning and location for usages of features that should be imported explicitly.
            "-unchecked", // Enable additional warnings where generated code depends on assumptions.
            "-language:implicitConversions", // Allow definition of implicit functions called views.
            "-language:strictEquality",
            //"-Xfatal-warnings", // Fail the compilation if there are any warnings.
            "-Wunused:implicits", // Warn if an implicit parameter is unused.
            "-Wunused:explicits",
            "-Wunused:imports", // Warn if an import selector is not referenced.
            "-Wunused:params", // Warn if a value parameter is unused.
        ),

        libraryDependencies += "org.typelevel" %% "cats-parse" % "1.1.0",

        libraryDependencies += "com.lihaoyi" %% "utest" % "0.9.1" % Test,
        
        
        testFrameworks += new TestFramework("utest.runner.Framework")
    )
