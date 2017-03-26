lazy val root = (project in file("."))
  .settings(
    name := "99Problems",
    scalaVersion := "2.12.1"
  )

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.1" % "test"
)