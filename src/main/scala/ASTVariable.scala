class ASTVariable(name: String, value: Float) extends ASTNumericNode {
	def eval: Float =  value
	override def toJson: ujson.Obj = {
		ujson.Obj(
			"type" -> "variable",
			"name" -> name,
			"value" -> value
		)
	}
}

object ASTVariable {
	def apply(name: String, value: Float): ASTVariable = new ASTVariable(name, value)
}