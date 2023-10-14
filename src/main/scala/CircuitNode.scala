import upickle.default.{ReadWriter, macroRW}

case class CircuitNode(name: String) {
	def combined(rhs: CircuitNode): CircuitNode =	CircuitNode(name + rhs.name)
	override def toString: String = name
}

object CircuitNode {
	def ground: CircuitNode = CircuitNode("0")

	implicit val rw: ReadWriter[CircuitNode] = macroRW
}