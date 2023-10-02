case class ASTResistor(
	cCons: ASTNode,
	valCons: ASTNumericNode
) extends ASTNode {
	def toJson: ujson.Obj = {
		ujson.Obj(
			"type" -> "Resistor",
			"constructor" -> cCons.toJson
		)
	}
}
