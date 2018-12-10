lazy val commonSettings = Seq(
    scalaVersion := "2.12.3",

    parallelExecution in Test := false,

    // Show warnings
    scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-Ywarn-unused", "-Xlint"),

    // Dependencies
    // Scalatest
    libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.4",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % "test",

    // Json4s
    libraryDependencies += "org.json4s" %% "json4s-native" % "3.6.0-M1"
)

lazy val core = (project in file("core"))
    .settings(
        commonSettings, 
        name := "MAD-core"
    )
    
lazy val spec = (project in file("spec"))
    .settings(
        commonSettings,
        name := "MAD-spec"
    ).dependsOn(core)

lazy val qa = (project in file("qa"))
    .settings(
        commonSettings,
        name := "MAD-QA"
    ).dependsOn(core, spec)
