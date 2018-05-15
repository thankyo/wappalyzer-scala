name := "wappalyzer-scala"

version := "0.1"

scalaVersion := "2.12.6"

libraryDependencies ++= Seq(
  "com.typesafe.play" %% "play-json" % "2.6.7",
  "com.typesafe.play" %% "play-ahc-ws-standalone" % "1.1.8",
  "com.typesafe.play" %% "play-ws-standalone-json" % "1.1.8",
  "org.specs2" %% "specs2-core" % "4.2.0" % "test"
)