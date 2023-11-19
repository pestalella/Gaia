ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "2.13.11"
ThisBuild / trackInternalDependencies := TrackLevel.TrackAlways

// Parallel collections support
libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4"
//libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.7.0"
libraryDependencies += "com.typesafe.akka" %% "akka-actor-typed" % "2.7.0"
libraryDependencies += "com.typesafe.akka" %% "akka-remote" % "2.7.0"
