ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "2.13.11"
ThisBuild / trackInternalDependencies := TrackLevel.TrackAlways

libraryDependencies += "com.lihaoyi" %% "ujson" % "3.1.2"

// Parallel collections support
libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4"

// Akka library
libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.7.0"
libraryDependencies += "com.typesafe.akka" %% "akka-remote" % "2.7.0"
libraryDependencies += "com.typesafe.akka" %% "akka-serialization-jackson" % "2.7.0"
//libraryDependencies += "com.fasterxml.jackson.core" %% "jackson-core-2.15.3"
//libraryDependencies += "com.typesafe.akka" %% "akka-protobuf" % "2.7.0"
libraryDependencies += "org.apache.spark" %% "spark-core" % "3.5.0"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.17" % "test"

ThisBuild / assemblyMergeStrategy := {
	case PathList("META-INF", xs@_*) => MergeStrategy.discard
	case x =>
		val oldStrategy = (ThisBuild / assemblyMergeStrategy).value
		oldStrategy(x)
}