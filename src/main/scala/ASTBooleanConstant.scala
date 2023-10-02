class ASTBooleanConstant(value: Boolean) extends ASTBooleanNode {
	def eval(): Boolean = value
	def toJson: ujson.Obj = {
		ujson.Obj(
			"type" -> "boolean",
			"value" -> value
		)
	}
}

object ASTBooleanConstant {
	def apply(v: Boolean): ASTBooleanConstant = new ASTBooleanConstant(v)
}