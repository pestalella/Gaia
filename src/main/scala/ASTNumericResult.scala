class ASTNumericResult(value: Float) {
}

object ASTNumericResult {
	def apply(v: Float): ASTNumericResult = new ASTNumericResult(v)
}