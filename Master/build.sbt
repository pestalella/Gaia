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

assemblyMergeStrategy in assembly := {
	case PathList("META-INF", xs@_*) => MergeStrategy.discard
	case x =>
		val oldStrategy = (assemblyMergeStrategy in assembly).value
		oldStrategy(x)
}