case class ASTSeries(cConsA: ASTNode, cConsB: ASTNode) extends ASTNode {
	def toJson: ujson.Obj = {
		ujson.Obj(
			"type" -> "Series",
			"cconsA" -> cConsA.toJson,
			"cconsB" -> cConsB.toJson
		)
	}
}
