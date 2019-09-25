val dottyVersion = "0.19.0-RC1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "polka",
    version := "0.1.0",

    scalaVersion := dottyVersion,
    scalacOptions += "-Yindent-colons",

    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"
  )

assemblyMergeStrategy in assembly := {
  case PathList("META-INF", xs @ _*) => MergeStrategy.discard
  case x => MergeStrategy.first
}
