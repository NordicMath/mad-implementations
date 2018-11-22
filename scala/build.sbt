scalaVersion := "2.12.3"

name := "MAD"

parallelExecution in Test := false

// Show warnings
scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

