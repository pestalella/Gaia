object ASTEnd extends ASTNode {
	def toJson: ujson.Obj = {
		ujson.Obj(
			"type" -> "End"
		)
	}
}

