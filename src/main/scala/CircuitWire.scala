case class CircuitWire(
	nodes: Seq[CircuitNode]
) extends CircuitComponent(nodes) {
	override def applyCommand(nodeCommand: ASTNode): Circuit = {
		nodeCommand match {
			case cmd: ASTCapacitor =>
				CircuitCapacitor(
					Seq(nodes.head, nodes.last),
					value = cmd.valCons.eval
				).applyCommand(cmd.cCons)
			case ASTEnd => Circuit(
					nodes = nodes,
					components = Seq(this)
				)
			case cmd: ASTParallel =>
				val lhs = this.applyCommand(cmd.cConsA)
				val rhs = CircuitWire(nodes).applyCommand(cmd.cConsB)
				lhs.combined(rhs)
			case cmd: ASTResistor =>
				CircuitResistor(
					Seq(nodes.head, nodes.last),
					value = cmd.valCons.eval
				).applyCommand(cmd.cCons)
			case cmd: ASTSeries =>
				val midNode = nodes.head.combined(nodes.last)
				val lhs = CircuitWire(Seq(nodes.head, midNode)).applyCommand(cmd.cConsA)
				val rhs = CircuitWire(Seq(midNode, nodes.last)).applyCommand(cmd.cConsB)
				lhs.combined(rhs)
			case _ => ???
		}
	}

	override def remapNodes(oldNodes: Seq[CircuitNode], newNode: CircuitNode): CircuitComponent = {
		CircuitWire(super.remapped(oldNodes, newNode))
	}

	override def toSpice: String = Seq(
		"W", nodes.head, nodes.last
	).mkString(" ")
}
