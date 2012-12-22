name := "PrimeNumberGenerator"

version := "1.0.0"

scalaVersion := "2.9.2"

scalacOptions ++= Seq("-unchecked", "-deprecation")

initialCommands in console := "import com.github.wpm.PrimeNumberGenerator._"

libraryDependencies += "org.scalatest" %% "scalatest" % "1.8" % "test"
