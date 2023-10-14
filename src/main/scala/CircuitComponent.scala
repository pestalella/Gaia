import upickle.default.{ReadWriter, macroRW}

sealed abstract class CircuitComponent {
	def nodes: Seq[CircuitNode]
	def remapNodes(nodes: Seq[CircuitNode], newNode: CircuitNode): CircuitComponent = ???
	def remapped(oldNodes: Seq[CircuitNode], newNode: CircuitNode): Seq[CircuitNode] = {
		for {n <- nodes} yield {
			if (oldNodes.contains(n)) newNode else n
		}
	}
	def copy(nodes: Seq[CircuitNode] = nodes) : CircuitComponent
	def toSpice: String = ???
}

object CircuitComponent {
//	implicit val rw: ReadWriter[CircuitComponent] = macroRW
	implicit val rw: ReadWriter[CircuitComponent] = ReadWriter.merge(
		CircuitCapacitor.rw,
 		CircuitResistor.rw,
		CircuitWire.rw,
	)
}

case class CircuitCapacitor(
	nodes: Seq[CircuitNode],
	value: Float,
	capacitorNumber: Int
) extends CircuitComponent {
	override def remapNodes(oldNodes: Seq[CircuitNode], newNode: CircuitNode): CircuitComponent = {
		CircuitCapacitor(super.remapped(oldNodes, newNode), value)
	}
	def copy(copyNodes: Seq[CircuitNode] = nodes): CircuitComponent =
		CircuitCapacitor(copyNodes, value)
	override def toSpice: String = Seq(
		"C" + capacitorNumber.toString, nodes.head, nodes.last, ValueUtils.valueToCapValue(value)
	).mkString(" ")
}

object CircuitCapacitor {
	private var counter = 0
	def apply(
		nodes: Seq[CircuitNode],
		value: Float,
		dummy: Int = 0
	): CircuitCapacitor = {
		counter += 1
		new CircuitCapacitor(nodes = nodes, value = value, capacitorNumber = counter)
	}
	// For testing purposes, should not be used in general code
	def reset(): Unit = {
		counter = 0
	}

	implicit val rw: ReadWriter[CircuitCapacitor] = macroRW
}

case class CircuitResistor(
	nodes: Seq[CircuitNode],
	value: Float,
	resistorNumber: Int
) extends CircuitComponent {
	override def remapNodes(oldNodes: Seq[CircuitNode], newNode: CircuitNode): CircuitComponent = {
		CircuitResistor(super.remapped(oldNodes, newNode), value)
	}
	def copy(copyNodes: Seq[CircuitNode] = nodes) : CircuitComponent =
		CircuitResistor(copyNodes, value)
	override def toSpice: String = Seq(
		"R" + resistorNumber.toString, nodes.head, nodes.last, ValueUtils.valueToResistorValue(value)
	).mkString(" ")
}

object CircuitResistor {
	private var counter = 0

	def apply(
		nodes: Seq[CircuitNode],
		value: Float,
		dummy: Int = 0
	): CircuitResistor = {
		counter += 1
		new CircuitResistor(nodes = nodes, value = value, resistorNumber = counter)
	}

	def reset(): Unit = {
		counter = 0
	}

	implicit val rw: ReadWriter[CircuitResistor] = macroRW
}

case class CircuitWire(
	nodes: Seq[CircuitNode]
) extends CircuitComponent {
	override def remapNodes(oldNodes: Seq[CircuitNode], newNode: CircuitNode): CircuitComponent = {
		CircuitWire(super.remapped(oldNodes, newNode))
	}
	def copy(copyNodes: Seq[CircuitNode] = nodes): CircuitComponent =
		CircuitWire(copyNodes)
	override def toSpice: String = Seq(
		"W", nodes.head, nodes.last
	).mkString(" ")
}

object CircuitWire {
	implicit val rw: ReadWriter[CircuitWire] = macroRW
}