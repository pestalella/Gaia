import GaiaCommon.{FitnessCalculator, SimDataPoint}

import java.io.{File, PrintWriter}
import java.nio.file.Files
import scala.sys.process._
import scala.util.{Failure, Success, Try}

object CircuitEvaluator {
	def calcFitness(circuitSpice: String, fitEval: FitnessCalculator): Double = {
		val simData = simulationResults(circuitSpice)
		if (simData.isEmpty)
			1E10
		else {
			fitEval.calc(simData)
		}
	}

	private def simulationResults(circuitSpice: String): Seq[SimDataPoint] =
		parseSimResult(
			simulateCircuit(circuitSpice)
		)

	private def simulateCircuit(circuitSpice: String): String = {
		val tempName = Try {
			Files.createTempFile("circuit_", ".cir").toString
		}
		val circuitFileName = tempName match {
			case Failure(e) =>
				println(s"Unable to create temporary file. Error was: ${e.getMessage}")
				""
			case Success(output) =>
				tempName.toString
		}
		if (circuitFileName.isEmpty) {
			""
		} else {
			new PrintWriter(circuitFileName) {
				write(circuitSpice)
				close()
			}
			val simCommand = s"ngspice  -b $circuitFileName"
			val result = Try {
				simCommand.!!(ProcessLogger(_ => ()))
			}
			result match {
				case Failure(e) =>
					println(s"Circuit $circuitFileName failed to run on ngspice. Error was: ${e.getMessage}")
					""
				case Success(output) =>
					new File(circuitFileName).delete()
					output
			}
		}
	}

	private def parseSimResult(simResult: String): Seq[SimDataPoint] = {
		val startPos = simResult indexOf "No. of Data Rows"
		if (startPos == -1)
			Seq()
		else {
			val trimmedLog = simResult.substring(simResult indexOf "No. of Data Rows")
			val lines = trimmedLog.replace("\t", " ").linesIterator.filterNot(_.trim.isEmpty).toList
			var state = 0
			lines.foldLeft(Seq(SimDataPoint(0, 0, 0)))((dataPoints, line) =>
				state match {
					case 0 =>
						if (line.contains("Index")) {
							state = 1
						}
						dataPoints
					case 1 =>
						if (line.contains("-------")) {
							state = 2
						}
						dataPoints
					case 2 => // Ready to start reading data points
						if (line.trim.isEmpty || line.contains("Index")) {
							state = 1
							dataPoints
						} else {
							val stringDataComponents = line.split("\\s+")
							val cleanDataComponents = stringDataComponents map (d => if (d.contains(",")) d.init else d)
							dataPoints :+ SimDataPoint(
								frequency = cleanDataComponents(1).toDouble,
								magnitude = cleanDataComponents(2).toDouble,
								phase = cleanDataComponents(3).toDouble
							)
						}
				}
			).tail
		}
	}
}
