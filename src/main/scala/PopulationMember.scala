import upickle.default.{ReadWriter, macroRW}

case class PopulationMember(
	circuit: Circuit,
	generator: ASTNode,
	fitness: Double
) {
	override def toString: String = {
		s"fitness: ${fitness}\ncircuit: \n${circuit.toUndecoratedSpice}"
	}
}
object PopulationMember {
	implicit val rw: ReadWriter[PopulationMember] = macroRW
}