case class Circuit(
	nodes: Seq[CircuitNode],
	components: Seq[CircuitComponent])
{
	def combined(rhs: Circuit): Circuit = {
		Circuit(
			nodes = nodes ++ rhs.nodes,
			components = components ++ rhs.components)
	}

	def cleanCircuit(externalNodes: Seq[CircuitNode]): Circuit = {
		// Remove wires, unifying the nodes they are attached to
		val wires = components collect { case w: CircuitWire => w }
		wires.foldLeft(this)((circuit, _) => circuit.removeWired(externalNodes))
	}

	def removeWired(externalNodes: Seq[CircuitNode]): Circuit = {
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
					Circuit(
						nodes = nodes,
						components = filteredComponents
					)
				else {
					val nodeA = wireToRemove.nodes.head
					val nodeB = wireToRemove.nodes.last
					val nodeToRemain = if (nodeA.name.length < nodeB.name.length) nodeA
						else (
							if (nodeA.name.length > nodeB.name.length) nodeB
							else (
								if (nodeA.name < nodeB.name) nodeA else nodeB
							)
						)
					val remappedComponents = filteredComponents map {
						_.remapNodes(wireToRemove.nodes, nodeToRemain)
					}
					val ret = Circuit(
						nodes = nodes.filter(!wireToRemove.nodes.contains(_)) :+ nodeToRemain,
						components = remappedComponents
					)
					println("new circuit:")
					println(ret.toSpice)
					ret
				}
			}
		}
	}

	def toSpice: String = {
		(for {
			component <- components
		} yield {
			component.toSpice
		}).mkString("\n")
	}
}
