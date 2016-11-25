

val circeVersion = "0.5.1"
val finchVersion = "0.11.0-M4"

lazy val dv = project
  .in(file("."))
  .settings(
    scalaVersion in ThisBuild := "2.11.8",

    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core",
      "io.circe" %% "circe-generic",
      "io.circe" %% "circe-parser"
    ).map(_ % circeVersion),

    libraryDependencies ++= Seq(
      "com.github.finagle" %% "finch-core",
      "com.github.finagle" %% "finch-circe"
    ).map(_ % finchVersion)
  )
