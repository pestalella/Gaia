import upickle.default.{ReadWriter, macroRW}

case class PopulationMember(
	circuit: Circuit,
	generator: ASTNode,
	fitness: Double
)
object PopulationMember {
	implicit val rw: ReadWriter[PopulationMember] = macroRW
}