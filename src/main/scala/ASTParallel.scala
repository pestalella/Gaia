case class ASTParallel(cConsA: ASTNode, cConsB: ASTNode) extends ASTNode {
	def toJson: ujson.Obj = {
		ujson.Obj(
			"type" -> "Parallel",
			"cconsA" -> cConsA.toJson,
			"cconsB" -> cConsB.toJson
		)
	}
}
