case class CircuitResistor(
	nodes: Seq[CircuitNode],
	value: Float,
	resistorNumber: Int
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
				val rhs = CircuitResistor(nodes, value).applyCommand(cmd.cConsB)
				lhs.combined(rhs)
			case cmd: ASTResistor =>
				CircuitResistor(
					Seq(nodes.head, nodes.last),
					value = cmd.valCons.eval
				).applyCommand(cmd.cCons)
			case cmd: ASTSeries =>
				val midNode = nodes.head.combined(nodes.last)
				val lhs = CircuitResistor(Seq(nodes.head, midNode), value).applyCommand(cmd.cConsA)
				val rhs = CircuitResistor(Seq(midNode, nodes.last), value).applyCommand(cmd.cConsB)
				lhs.combined(rhs)
		}
	}
	override def remapNodes(oldNodes: Seq[CircuitNode], newNode: CircuitNode): CircuitComponent = {
		CircuitResistor(super.remapped(oldNodes, newNode), value)
	}
	override def toSpice: String = Seq(
		"R" + resistorNumber.toString, nodes.head, nodes.last, value.toString
	).mkString(" ")
}

object CircuitResistor {
	private var counter = 0
	def apply(
		nodes: Seq[CircuitNode],
		value: Float
	): CircuitResistor = {
		counter += 1
		CircuitResistor(nodes, value, counter)
	}
	// For testing purposes, should not be used in general code
	def reset(): Unit = {
		counter = 0
	}
}