val scala3 = "3.5.2"
val munit = "1.0.3"

lazy val root = project
  .in(file("."))
  .settings(
    name := "advent-2024",
    version := "0.1.0",
    scalaVersion := scala3,
    libraryDependencies += "org.scalameta" %% "munit" % munit % Test
  )
