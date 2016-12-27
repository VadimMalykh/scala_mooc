name := "bayesian_network"

version := "1.0"

//scalaVersion := "2.12.1"

libraryDependencies += "org.specs2" %% "specs2-core" % "3.8.5" % "test"
libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.1"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"

scalacOptions in Test ++= Seq("-Yrangepos")
