import ujson._

case class PopulationMember(
	circuit: Circuit,
	generator: ASTNode,
	fitness: Double,
	circuitId: Int
) {
	override def toString: String = s"fitness: $fitness\ncircuit: \n${circuit.toUndecoratedSpice}"
	def toJson: Obj = Obj(
		"circuit" -> circuit.toJson,
		"generator" -> generator.toJson,
		"fitness" -> fitness.toString,
		"circuit_id" -> circuitId.toString
	)
}

object PopulationMember {
	var lastCircuitId = 0

	def apply(
		circuit: Circuit,
		generator: ASTNode,
		fitness: Double
	): PopulationMember = {
		lastCircuitId += 1
		new PopulationMember(
			circuit = circuit,
			generator = generator,
			fitness = fitness,
			circuitId = lastCircuitId)
	}
	def fromJson(obj: Obj): PopulationMember = {
		PopulationMember(
			circuit = Circuit.fromJson(obj("circuit").obj),
			generator = ASTNode.fromJson(obj("generator").obj),
			fitness = obj("fitness").str.toDouble,
			circuitId = obj("circuit_id").str.toInt
		)
	}
}
