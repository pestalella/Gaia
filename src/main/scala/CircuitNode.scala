case class CircuitNode(name: String) {
	def combined(rhs: CircuitNode): CircuitNode =	CircuitNode(name + rhs.name)
	override def toString: String = name
}
