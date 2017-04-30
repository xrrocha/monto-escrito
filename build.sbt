name := "monto-escrito"
version := "0.1.0"

scalaVersion := "2.12.1"

scalacOptions in ThisBuild ++= Seq(
  "-unchecked",
  "-deprecation",
  "-feature",
  "-language:implicitConversions"
)

libraryDependencies ++= Seq(
  "com.lihaoyi" %% "utest" % "0.4.5" % "test"
)

testFrameworks += new TestFramework("utest.runner.Framework")
