import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "org.ml4ai",
      scalaVersion := "2.12.7",
      version      := "0.1.0-SNAPSHOT"
    )),
    scalacOptions ++= Seq(
      "-feature",
    ),
    name := "WikiHopFR",
    libraryDependencies ++= Seq(
      "org.json4s" %% "json4s-jackson" % "3.6.3",
      "org.clulab" %% "processors-main" % "7.4.2",
      "org.clulab" %% "processors-modelsmain" % "7.4.2",
      "org.clulab" %% "processors-corenlp" % "7.4.2",
      "org.clulab" %% "processors-modelscorenlp" % "7.4.2",
      "com.typesafe" % "config" % "1.3.3",
      "org.apache.lucene" % "lucene-core" % "7.6.0",
      "org.apache.lucene" % "lucene-analyzers-common" % "7.6.0",
      "org.apache.lucene" % "lucene-queryparser" % "7.6.0",
      scalaTest % Test
    )
  )
