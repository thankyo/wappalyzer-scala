organization := "com.github.thankyo"
name := "wappalyzer-scala"

version := "0.0.3"
versionScheme := Some("semver-spec")

scalaVersion := "2.13.5"

libraryDependencies ++= Seq(
  "com.typesafe.play" %% "play-json" % "2.9.2",
  "com.typesafe.play" %% "play-ahc-ws-standalone" % "2.1.3",
  "com.typesafe.play" %% "play-ws-standalone-json" % "2.1.3",
  "org.specs2" %% "specs2-core" % "4.10.6" % "test"
)

licenses ++= Seq("Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0.html"))

githubOwner := "thankyo"
githubRepository := "wappalyzer-scala"
publishMavenStyle := true