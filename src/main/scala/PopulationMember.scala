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
