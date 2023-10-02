class CircuitComponent(nodes: Seq[CircuitNode]) {
	def applyCommand(cmd: ASTNode) : Circuit = ???
	def remapNodes(nodes: Seq[CircuitNode], newNode: CircuitNode): CircuitComponent = ???
	def remapped(oldNodes: Seq[CircuitNode], newNode: CircuitNode): Seq[CircuitNode] = {
		for {n <- nodes} yield {
			if (oldNodes.contains(n)) newNode else n
		}
	}
	def toSpice: String = ???
}
