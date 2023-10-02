class ASTNumericConstant(value: Float) extends ASTNumericNode {
	def eval: Float = value
	override def toJson: ujson.Obj = {
		ujson.Obj(
			"type" -> "constant",
			"value" -> value
		)
	}
}

object ASTNumericConstant {
	def apply(v: Float): ASTNumericConstant = new ASTNumericConstant(v)
}