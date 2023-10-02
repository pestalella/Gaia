case class ASTList(circuitConstructors: Seq[ASTNode]) extends ASTNode {
	def toJson: ujson.Obj = {
		ujson.Obj(
			"type" -> "list",
			"constructors" -> ujson.Arr(
				circuitConstructors map (_.toJson)
			)
		)
	}
}
