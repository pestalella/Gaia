import ujson.Obj
import upickle.default._

sealed trait ASTNode {
	def toJson: ujson.Obj
}
object ASTNode{
  implicit val rw: ReadWriter[ASTNode] = ReadWriter.merge(
		ASTCapacitor.rw,
		ASTResistor.rw,
		ASTParallel.rw,
		ASTSeries.rw,
		ASTThreeGND.rw,
		ASTNumericNode.rw,
		ASTNumericConstant.rw,
		ASTEnd.rw
	)
}

trait ASTNumericNode extends ASTNode {
	override def toJson: Obj = ujson.Obj()

	def eval: Float
}

object ASTNumericNode {
	//	implicit val rw: ReadWriter[ASTNumericNode] = macroRW
	//implicit def rw[T: ReadWriter]: ReadWriter[ASTNumericNode] = macroRW
	implicit val rw: ReadWriter[ASTNumericNode] = ReadWriter.merge(ASTNumericConstant.rw)
}

case class ASTNumericConstant(value: Float) extends ASTNumericNode {
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

	//implicit def rw[T: ReadWriter]: ReadWriter[ASTNumericConstant] = macroRW
	implicit val rw: ReadWriter[ASTNumericConstant] = macroRW
}

case class ASTCapacitor(
	cCons: ASTNode,
	valCons: ASTNumericNode
) extends ASTNode {
	def toJson: ujson.Obj = {
		ujson.Obj(
			"type" -> "Capacitor",
			"constructor" -> cCons.toJson,
			"valueCons" -> valCons.toJson
		)
	}
}

case class ASTResistor(
	cCons: ASTNode,
	valCons: ASTNumericNode
) extends ASTNode {
	def toJson: ujson.Obj = {
		ujson.Obj(
			"type" -> "Resistor",
			"constructor" -> cCons.toJson,
			"valueCons" -> valCons.toJson
		)
	}
}
object ASTResistor {
	implicit val rw: ReadWriter[ASTResistor] = macroRW
}

object ASTCapacitor {
	implicit val rw: ReadWriter[ASTCapacitor] = macroRW
}

case class ASTThreeGND(
	aCons: ASTNode,
	bCons: ASTNode,
	gndCons: ASTNode
) extends ASTNode {
	def toJson: ujson.Obj = {
		ujson.Obj(
			"type" -> "ThreeGND",
			"aCons" -> aCons.toJson,
			"bCons" -> bCons.toJson,
			"gndCons" -> gndCons.toJson
		)
	}
}

object ASTThreeGND {
	implicit val rw: ReadWriter[ASTThreeGND] = macroRW
}

case class ASTParallel(cConsA: ASTNode, cConsB: ASTNode) extends ASTNode {
	def toJson: ujson.Obj = {
		ujson.Obj(
			"type" -> "Parallel",
			"cconsA" -> cConsA.toJson,
			"cconsB" -> cConsB.toJson
		)
	}
}

object ASTParallel {
	implicit val rw: ReadWriter[ASTParallel] = macroRW
}

case class ASTSeries(cConsA: ASTNode, cConsB: ASTNode) extends ASTNode {
	def toJson: ujson.Obj = {
		ujson.Obj(
			"type" -> "Series",
			"cconsA" -> cConsA.toJson,
			"cconsB" -> cConsB.toJson
		)
	}
}
object ASTSeries {
	implicit val rw: ReadWriter[ASTSeries] = macroRW
}

class ASTEnd extends ASTNode {
	def toJson: ujson.Obj = {
		ujson.Obj(
			"type" -> "End"
		)
	}
}

object ASTEnd {
	def apply() = new ASTEnd;
	implicit val rw: ReadWriter[ASTEnd] = macroRW
}

