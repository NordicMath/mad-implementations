scalaVersion := "2.12.3"

name := "MAD"

parallelExecution in Test := false

// Show warnings
scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

// Dependencies
// Scalatest
libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.4"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % "test"

