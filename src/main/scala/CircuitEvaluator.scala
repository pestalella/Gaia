import java.io.File
import scala.util.{Try, Failure, Success}

object CircuitEvaluator {
	def calcFitness(circuit: Circuit, fitEval: FitnessCalculator): Double = {
		val simData = simulationResults(circuit)
		if (simData.isEmpty)
			1E10
		else {
			fitEval.calc(simData) * (1.0 + circuit.components.size * 0.00001)
		}
	}

	private def simulationResults(circuit: Circuit): Seq[SimDataPoint] =
		parseSimResult(
			simulateCircuit(circuit)
		)

	private def simulateCircuit(circuit: Circuit): String = {
		circuit.writeCircuit()
		import scala.sys.process._
		val simCommand = s"ngspice  -b test_${circuit.circuitNumber}.cir"
		val result = Try {
			simCommand.!!(ProcessLogger(_ => ()))
		}
		result match {
			case Failure(e) =>
				println(s"Circuit ${circuit.circuitNumber} failed to run on ngspice. Error was: ${e.getMessage}")
				""
			case Success(output) =>
				new File(s"test_${circuit.circuitNumber}.cir").delete()
				output
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
