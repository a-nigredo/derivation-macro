lazy val commonSettings = publishSettings ++ Seq(
  //used as sonatype profile
  organization := "com.github.a-nigredo",
  scalaVersion := "2.12.6",
  libraryDependencies := {
    Seq("org.specs2" %% "specs2-core" % "4.0.2" % "test",
      scalaVersion("org.scala-lang" % "scala-reflect" % _).value,
      compilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full))
  },
  crossScalaVersions := Seq("2.11.12", scalaVersion.value)
)

lazy val root = (project in file(".")).settings(commonSettings: _*).settings(name := "derivation_macro")

releaseUseGlobalVersion := false

val publishSettings = Seq(
  publishTo := sonatypePublishTo.value,
  publishMavenStyle := true,
  publishArtifact in Test := false,
  pomIncludeRepository := { _ => false },
  pomExtra := {
    <url>https://github.com/a-nigredo/derivation-macro</url>
      <licenses>
        <license>
          <name>Apache License, Version 2.0</name>
          <url>http://apache.org/licenses/LICENSE-2.0</url>
          <distribution>repo</distribution>
        </license>
      </licenses>
      <scm>
        <url>git@github.com:a-nigredo/derivation-macro.git</url>
        <connection>scm:git:git@github.com:a-nigredo/derivation-macro.git</connection>
      </scm>
      <developers>
        <developer>
          <id>ivanov</id>
          <name>Andrey Ivanov</name>
          <email>a.nigredo@gmail.com</email>
        </developer>
      </developers>
  }
)

import ReleaseTransformations._

releaseCrossBuild := true
releaseProcess := Seq[ReleaseStep](
  checkSnapshotDependencies,
  inquireVersions,
  runClean,
  runTest,
  setReleaseVersion,
  commitReleaseVersion,
  tagRelease,
  releaseStepCommandAndRemaining("+publishSigned"),
  setNextVersion,
  commitNextVersion,
  releaseStepCommand("sonatypeReleaseAll"),
  pushChanges
)