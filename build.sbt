//ThisBuild / version := "0.1.0-SNAPSHOT"
//
//ThisBuild / scalaVersion := "2.13.11"
//
//lazy val root = (project in file("."))
//  .settings(
//    name := "Gaia"
//  )
//
//// https://mvnrepository.com/artifact/com.lihaoyi/upickle
//libraryDependencies += "com.lihaoyi" %% "upickle" % "3.1.2"
//libraryDependencies ++= Seq(
//	"org.scalatest" %% "scalatest" % "latest.integration" % Test
//)
//// Parallel collections support
//libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4"
//
//// Doodle, for charts and graphics
//libraryDependencies += "org.creativescala" %% "doodle" % "0.19.0"
//
//
//

ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "2.13.11"
ThisBuild / trackInternalDependencies := TrackLevel.TrackAlways

libraryDependencies += "com.lihaoyi" %% "ujson" % "3.1.2"

//lazy val root = (project in file("."))
//	.settings(
//		name := "RemoteFitnessEvaluator"
//	)
//	.dependsOn(GaiaCommon, FitnessEvaluatorRemote)
//	.aggregate(GaiaCommon, FitnessEvaluatorRemote)

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

