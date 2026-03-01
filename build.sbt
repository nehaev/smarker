import scala.io.Source
import xerial.sbt.Sonatype.sonatypeCentralHost


val scala3Version = "3.3.7"

lazy val root = project
    .in(file("."))
    .settings(
        name := "smarker",
        version := Source.fromFile("VERSION").getLines.next,

        scalaVersion := scala3Version,

        startYear := Some(2026),
        organization := "io.github.nehaev",
        homepage := Some(url("https://github.com/nehaev/smarker")),
        licenses:= Seq(("BSD Software License, 2-clause version", url("https://github.com/nehaev/smarker/blob/main/LICENSE"))),
        developers := List(
            Developer("nehaev", "Anton Nekhaev", "nehaev@gmail.com", url("https://github.com/nehaev"))
        ),
        versionScheme := Some(VersionScheme.EarlySemVer),
        scmInfo := Some(
            ScmInfo(
                url("https://github.com/nehaev/smarker"),
                "scm:git@github.com:nehaev/smarker.git"
            )
        ),

        publishTo := {
            if (isSnapshot.value)
                Some(MavenCache("local-maven", file("/tmp/mvn-snapshots")))
            else
                sonatypePublishToBundle.value
        },
        publishMavenStyle := true,
        sonatypeCredentialHost := sonatypeCentralHost,


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
