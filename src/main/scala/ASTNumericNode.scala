import ujson.Obj
abstract class ASTNumericNode extends ASTNode {
	override def toJson: Obj = ujson.Obj()
	def eval: Float
}
