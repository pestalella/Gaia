case class ASTCapacitor(
	cCons: ASTNode,
	valCons: ASTNumericNode
) extends ASTNode {
	def toJson: ujson.Obj = {
		ujson.Obj(
			"type" -> "Capacitor",
			"constructor" -> cCons.toJson
		)
	}
}

