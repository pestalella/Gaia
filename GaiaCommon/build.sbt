ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.12"

libraryDependencies += "com.typesafe.akka" %% "akka-serialization-jackson" % "2.7.0" % "provided"

//assemblyMergeStrategy in assembly := {
//	case PathList("META-INF", _*) => MergeStrategy.discard
//	case _ => MergeStrategy.first
//}