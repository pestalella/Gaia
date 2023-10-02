case class CircuitCapacitor(
	nodes: Seq[CircuitNode],
	value: Float,
	capacitorNumber: Int
) extends CircuitComponent(nodes) {
	override def applyCommand(nodeCommand: ASTNode) : Circuit =	{
		nodeCommand match {
			case cmd: ASTCapacitor =>
				CircuitCapacitor(
					Seq(nodes.head, nodes.last),
					value = cmd.valCons.eval
				).applyCommand(cmd.cCons)
			case ASTEnd =>
				Circuit(
					nodes = nodes,
					components = Seq(this)
				)
			case cmd: ASTParallel =>
				val lhs = this.applyCommand(cmd.cConsA)
				val rhs = CircuitCapacitor(nodes, value).applyCommand(cmd.cConsB)
				lhs.combined(rhs)
			case cmd: ASTResistor =>
				CircuitResistor(
					Seq(nodes.head, nodes.last),
					value = cmd.valCons.eval
				).applyCommand(cmd.cCons)
			case cmd: ASTSeries =>
				val midNode = nodes.head.combined(nodes.last)
				val lhs = CircuitCapacitor(Seq(nodes.head, midNode), value).applyCommand(cmd.cConsA)
				val rhs = CircuitCapacitor(Seq(midNode, nodes.last), value).applyCommand(cmd.cConsB)
				lhs.combined(rhs)
		}
	}
	override def remapNodes(oldNodes: Seq[CircuitNode], newNode: CircuitNode): CircuitComponent = {
		CircuitCapacitor(super.remapped(oldNodes, newNode), value)
	}
	override def toSpice: String = Seq(
		"C" + capacitorNumber.toString, nodes.head, nodes.last, value.toString
	).mkString(" ")
}

object CircuitCapacitor {
	private var counter = 0
	def apply(
		nodes: Seq[CircuitNode],
		value: Float
	): CircuitCapacitor = {
		counter += 1
		CircuitCapacitor(nodes, value, counter)
	}
	// For testing purposes, should not be used in general code
	def reset(): Unit = {
		counter = 0
	}
}