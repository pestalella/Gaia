import ujson.Obj

sealed abstract class CircuitComponent {
	def nodes: Seq[CircuitNode]
	def remapNodes(nodes: Seq[CircuitNode], newNode: CircuitNode): CircuitComponent
	def remapped(oldNodes: Seq[CircuitNode], newNode: CircuitNode): Seq[CircuitNode] = {
		for {n <- nodes} yield {
			if (oldNodes.contains(n)) newNode else n
		}
	}
	def copy(nodes: Seq[CircuitNode] = nodes) : CircuitComponent
	def toSpice: String
	def toJson: ujson.Obj
}

object CircuitComponent {
	def fromJson(inputJson: Obj): CircuitComponent = {
		val cType = inputJson("type").str
		if (cType == "Capacitor")
			CircuitCapacitor.fromJson(inputJson)
		else if (cType == "Resistor")
			CircuitResistor.fromJson(inputJson)
		else if (cType == "Inductor")
			CircuitInductor.fromJson(inputJson)
		else {
			println(s"OMG, got a component of type $cType")
			CircuitResistor(Seq(), 0)
		}
	}
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

	override def toJson: Obj = Obj(
		"type" -> "Capacitor",
		"nodes" -> ujson.Arr.from(nodes map (_.toJson)),
		"value" -> value.toString,
		"ident" -> capacitorNumber
	)
}

object CircuitCapacitor {
	private var counter = 0

	def apply(
		nodes: Seq[CircuitNode],
		value: Float
	): CircuitCapacitor = {
		counter += 1
		new CircuitCapacitor(nodes = nodes, value = value, capacitorNumber = counter)
	}

	// For testing purposes, should not be used in general code
	def reset(): Unit = {
		counter = 0
	}

	def fromJson(inputJson: Obj): CircuitComponent = {
		val inputNodes = (for (node <- inputJson("nodes").arr) yield {
			CircuitNode.fromJson(node.obj)
		}).toSeq
		CircuitCapacitor(
			nodes = inputNodes,
			value = inputJson("value").str.toFloat,
		)
	}
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

	override def toJson: Obj = Obj(
		"type" -> "Resistor",
		"nodes" -> ujson.Arr.from(nodes map (_.toJson)),
		"value" -> value.toString,
		"ident" -> resistorNumber
	)
}

object CircuitResistor {
	private var counter = 0

	def apply(
		nodes: Seq[CircuitNode],
		value: Float
	): CircuitResistor = {
		counter += 1
		new CircuitResistor(nodes = nodes, value = value, resistorNumber = counter)
	}

	def reset(): Unit = {
		counter = 0
	}

	def fromJson(inputJson: Obj): CircuitComponent = {
		val inputNodes = (for (node <- inputJson("nodes").arr) yield {
			CircuitNode.fromJson(node.obj)
		}).toSeq
		CircuitResistor(
			nodes = inputNodes,
			value = inputJson("value").str.toFloat,
		)
	}
}

case class CircuitInductor(
	nodes: Seq[CircuitNode],
	value: Float,
	inductorNumber: Int
) extends CircuitComponent {
	override def remapNodes(oldNodes: Seq[CircuitNode], newNode: CircuitNode): CircuitComponent = {
		CircuitInductor(super.remapped(oldNodes, newNode), value)
	}
	def copy(copyNodes: Seq[CircuitNode] = nodes) : CircuitComponent =
		CircuitInductor(copyNodes, value)
	override def toSpice: String = Seq(
		"L" + inductorNumber.toString, nodes.head, nodes.last, ValueUtils.valueToInductorValue(value)
	).mkString(" ")

	override def toJson: Obj = Obj(
		"type" -> "Inductor",
		"nodes" -> ujson.Arr.from(nodes map (_.toJson)),
		"value" -> value.toString,
		"ident" -> inductorNumber
	)

}

object CircuitInductor {
	private var counter = 0

	def apply(
		nodes: Seq[CircuitNode],
		value: Float
	): CircuitInductor = {
		counter += 1
		new CircuitInductor(nodes = nodes, value = value, inductorNumber = counter)
	}

	def reset(): Unit = {
		counter = 0
	}

	def fromJson(inputJson: Obj): CircuitComponent = {
		val inputNodes = (for (node <- inputJson("nodes").arr) yield {
			CircuitNode.fromJson(node.obj)
		}).toSeq
		CircuitInductor(
			nodes = inputNodes,
			value = inputJson("value").str.toFloat,
		)
	}
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

	override def toJson: Obj = Obj(
		"type" -> "Wire",
		"nodes" -> ujson.Arr.from(nodes map (_.toJson)),
	)
}
