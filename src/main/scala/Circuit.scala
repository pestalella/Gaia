import java.io.{File, PrintWriter}
import upickle.default.{ReadWriter, macroRW}

class Circuit(
	val nodes: Seq[CircuitNode],
	val components: Seq[CircuitComponent],
	val circuitNumber: Int) {
	def combined(rhs: Circuit): Circuit = new Circuit(
		nodes = (nodes ++ rhs.nodes).distinct,
		components = components ++ rhs.components,
		circuitNumber = circuitNumber
	)
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
		val simData = parseSimResult(simResult)
		val cutOffFreq = 2000.0; // 2 kHz
		simData.foldLeft(0.0)((fitAccum, dataPoint) =>
			if (dataPoint.frequency < cutOffFreq)
				fitAccum + scala.math.pow(1.0 - dataPoint.magnitude, 2.0)
			else
				fitAccum + scala.math.pow(dataPoint.magnitude, 2.0)
		) + (components.size*0.1)
	}
	private def simulateCircuit(): String = {
		writeCircuit()
		import scala.sys.process._
		val simCommand = s"ngspice  -b test_$circuitNumber.cir"
		val output = simCommand.!!(ProcessLogger(_ => ()))
		new File(s"test_$circuitNumber.cir").delete()
		output
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
						//          println(s"data: ${dataComponents.mkString(":")}")
						//          println(s"index: ${dataComponents(0)} freq: ${dataComponents(1)}  mag: ${dataComponents(2)} phase: ${dataComponents(3)}")
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
			"VIN A 0 DC 0 AC 1",
			"RLOAD B 0 1k",
			toUndecoratedSpice,
			".AC DEC 20 10 100k",
			".PRINT AC V(B)",
			".OPTION NOACCT",
			".END"
		).mkString("\n")
	}
}

object Circuit {
	private var circuitCount = 0
	def apply(nodes: Seq[CircuitNode], components: Seq[CircuitComponent], dummy:Int = 0): Circuit = {
		circuitCount += 1
		new Circuit(nodes = nodes, components= components, circuitNumber = circuitCount)
	}

	implicit val rw: ReadWriter[Circuit] = macroRW
}