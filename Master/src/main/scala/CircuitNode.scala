case class CircuitNode(name: String) {
	def combined(rhs: CircuitNode): CircuitNode =	CircuitNode(name + rhs.name)
	override def toString: String = name
	def toJson: ujson.Obj = ujson.Obj(
		"name" -> name
	)
}

object CircuitNode {
	def ground: CircuitNode = CircuitNode("0")
	def via0: CircuitNode = CircuitNode("VIA0")

	def via1: CircuitNode = CircuitNode("VIA1")

	def fromJson(inputJson: ujson.Obj): CircuitNode =
		CircuitNode(name = inputJson("name").toString())
}