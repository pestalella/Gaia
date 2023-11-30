ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "2.13.11"
ThisBuild / trackInternalDependencies := TrackLevel.TrackAlways

// Parallel collections support
libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4"
libraryDependencies += "org.slf4j" % "slf4j-simple" % "2.0.9" % Test
libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.7.0"
libraryDependencies += "com.typesafe.akka" %% "akka-remote" % "2.7.0"
libraryDependencies += "com.typesafe.akka" %% "akka-serialization-jackson" % "2.7.0"

ThisBuild / assemblyMergeStrategy := {
	case PathList("META-INF", xs@_*) => MergeStrategy.discard
	case x =>
		val oldStrategy = (ThisBuild / assemblyMergeStrategy).value
		oldStrategy(x)
}