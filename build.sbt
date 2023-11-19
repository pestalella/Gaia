ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "2.13.11"
ThisBuild / trackInternalDependencies := TrackLevel.TrackAlways

libraryDependencies += "com.lihaoyi" %% "ujson" % "3.1.2"

lazy val GaiaCommon = project in file("GaiaCommon")
lazy val FitnessEvaluatorRemote = (project in file("FitnessEvaluatorRemote"))
	.settings(
		mainClass := Some("RemoteFitnessEvaluator")
	)
	.dependsOn(GaiaCommon)

lazy val Master = (project in file("Master"))
	.settings(
		mainClass := Some("Main")
	)
	.dependsOn(GaiaCommon)

