import java.io.PrintWriter
import ujson._

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
			"GNUPLOT circuitV V(B)",
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

	def fromJson(inputJson: Obj): Circuit = {
		val inputNodes = (for (node <- inputJson("nodes").arr) yield {
			CircuitNode.fromJson(node.obj)
		}).toSeq
		val inputComponents = (for (component <- inputJson("components").arr) yield {
			CircuitComponent.fromJson(component.obj)
		}).toSeq
		Circuit(
			nodes = inputNodes,
			components = inputComponents
			)
	}

}