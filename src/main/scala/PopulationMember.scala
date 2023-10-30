import ujson._

case class PopulationMember(
	circuit: Circuit,
	generator: ASTNode,
	fitness: Double
) {
	override def toString: String = s"fitness: $fitness\ncircuit: \n${circuit.toUndecoratedSpice}"
	def toJson: Obj = Obj(
		"circuit" -> circuit.toJson,
		"generator" -> generator.toJson,
		"fitness" -> fitness.toString
	)
}

object PopulationMember {
	def fromJson(obj: Obj): PopulationMember = {
		PopulationMember(
			circuit = Circuit.fromJson(obj("circuit").obj),
			generator = ASTNode.fromJson(obj("generator").obj),
			fitness = obj("fitness").str.toDouble
		)
	}
}
