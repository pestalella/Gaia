import java.io.{File, PrintWriter}
import scala.util.{Try, Failure, Success}

class Circuit(
	val nodes: Seq[CircuitNode],
	val components: Seq[CircuitComponent],
	val circuitNumber: Int) {

	def combined(rhs: Circuit): Circuit = {
		val newCirc = new Circuit(
			nodes = (nodes ++ rhs.nodes).distinct,
			components = components ++ rhs.components,
			circuitNumber = circuitNumber
		)
		newCirc
	}
	def cleanCircuit(externalNodes: Seq[CircuitNode]): Circuit = {
		// Remove wires, unifying the nodes they are attached to
		val wires = components collect { case w: CircuitWire => w }
		val noWires = wires.foldLeft(this)((circuit, _) => circuit.removeWired(externalNodes))
		new Circuit(
			nodes = noWires.nodes,
			components = noWires.components filter(comp => comp.nodes.distinct.length != 1),
			circuitNumber = circuitNumber
		)
	}

	private def removeWired(externalNodes: Seq[CircuitNode]): Circuit = {
		if (components.isEmpty)
			this
		else {
			val circuitWires = components collect { case w: CircuitWire => w }
			if (circuitWires.isEmpty)
				this
			else {
				val wireToRemove = circuitWires.head
				val nodeCandidates = wireToRemove.nodes filter {!externalNodes.contains(_)}
				val filteredComponents = components filter {_ != wireToRemove}
				if (nodeCandidates.isEmpty)
					new Circuit(
						nodes = nodes,
						components = filteredComponents,
						circuitNumber = circuitNumber
					)
				else {
					val nodeA = wireToRemove.nodes.head
					val nodeB = wireToRemove.nodes.last
					val nodeToRemain = if (nodeA.name.length < nodeB.name.length) nodeA
						else if (nodeA.name.length > nodeB.name.length) nodeB
						else if (nodeA.name < nodeB.name) nodeA else nodeB
					val remappedComponents = filteredComponents map {
						_.remapNodes(wireToRemove.nodes, nodeToRemain)
					}
					new Circuit(
						nodes = nodes.filter(!wireToRemove.nodes.contains(_)) :+ nodeToRemain,
						components = remappedComponents,
						circuitNumber = circuitNumber
					)
				}
			}
		}
	}

	def calcFitness(): Double = {
		val simResult = simulateCircuit()
		if (simResult.isEmpty)
			1E10
		else {
			val simData = parseSimResult(simResult)
			val dbData = simData map (dataPoint => {
				val dbMagnitude = 20*scala.math.log10(dataPoint.magnitude + 0.00001)
				SimDataPoint(
					frequency = dataPoint.frequency,
					magnitude = if (dbMagnitude.isNaN) 1E10 else dbMagnitude,
					phase = dataPoint.phase)
			})
			dbData.foldLeft(0.0)((fitAccum, dataPoint) => fitAccum + dataPointFitness(dataPoint)) *
				(1.0 + components.size*0.00001)
		}
	}

	private def dataPointFitness(simDataPoint: SimDataPoint): Double = {
		val hiCutOffFreq = 2000.0
		val lowCutOffFreq = 0.0
		if (lowCutOffFreq < simDataPoint.frequency && simDataPoint.frequency < hiCutOffFreq) {
			if (scala.math.abs(simDataPoint.magnitude) < 0.0001)
				0
			else if (scala.math.abs(simDataPoint.magnitude) < 0.6)
				scala.math.abs(simDataPoint.magnitude) * 10
			else
				scala.math.abs(simDataPoint.magnitude) * 100
		} else {
			if (simDataPoint.magnitude < -120.0)
				0
			else if (simDataPoint.magnitude < -60.0)
				scala.math.abs(-120 - simDataPoint.magnitude)
			else
				scala.math.abs(-120 - simDataPoint.magnitude) * 10
		}
	}

	private def simulateCircuit(): String = {
		writeCircuit()
		import scala.sys.process._
		val simCommand = s"ngspice  -b test_$circuitNumber.cir"
		val result = Try { simCommand.!!(ProcessLogger(_ => ()))}
  	result match {
	    case Failure(e) =>
				println(s"Circuit $circuitNumber failed to run on ngspice. Error was:\n${e.getMessage}")
				""
  	  case Success(output) =>
				new File(s"test_$circuitNumber.cir").delete()
				output
  	}
	}

	private def writeCircuit(): Unit = {
		new PrintWriter(s"test_$circuitNumber.cir") {
			write(toSpice)
			close()
		}
	}
	private def parseSimResult(simResult: String): Seq[SimDataPoint] = {
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

	def toUndecoratedSpice: String = {
		(for {
			component <- components
		} yield {
			component.toSpice
		}).mkString("\n")
	}

	def toSpice: String = {
		Seq(
			s".TITLE TEST CIRCUIT $circuitNumber",
			"",
			"VIN VS 0 DC 0 AC 1",
			"RSOURCE VS A 1k",
			"RLOAD B 0 1k",
			toUndecoratedSpice,
			s".AC DEC ${Parameters.simulationDataPoints} 10 200k",
			".PRINT AC V(B)",
			".OPTION NOACCT",
			".END"
		).mkString("\n")
	}

	def toSpicePlot: String = {
		Seq(
			s".TITLE TEST CIRCUIT $circuitNumber",
			"",
			"VIN A 0 DC 0 AC 1",
			"RLOAD B 0 1k",
			toUndecoratedSpice,
			".AC DEC 1 10 100",
			".PRINT AC V(B)",
			".CONTROL",
			s"AC DEC ${Parameters.simulationDataPoints} 10 200k",
			"GNUPLOT circuitV db(B)",
			"OPTION NOACCT",
			".ENDC",
			".END"
		).mkString("\n")
	}

	def toJson: ujson.Obj = {
		ujson.Obj(
			"nodes" -> ujson.Arr.from(nodes map (_.toJson)),
			"components" -> ujson.Arr.from(components map (_.toJson)),
			"circuitNumber" -> circuitNumber
		)
	}

	def writeToFile(fileName: String): Unit = {
		new PrintWriter(fileName) {
			write(toSpice)
			close()
		}
	}

}

object Circuit {
	private var circuitCount = 0
	def apply(nodes: Seq[CircuitNode], components: Seq[CircuitComponent]): Circuit = {
		circuitCount += 1
		new Circuit(nodes = nodes, components= components, circuitNumber = circuitCount)
	}
	def reset(): Unit = {
		circuitCount = 0
	}
}