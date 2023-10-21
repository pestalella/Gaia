import upickle.default._

import scala.runtime.Nothing$

sealed trait ASTNode {
	def nodeCount: Int
	def getNthSubtree(subtree: Int): ASTNode
	def withInsertion(toInsert: ASTNode, insertionPoint: Int): ASTNode
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
	def eval: Float
}

object ASTNumericNode {
	//	implicit val rw: ReadWriter[ASTNumericNode] = macroRW
	//implicit def rw[T: ReadWriter]: ReadWriter[ASTNumericNode] = macroRW
	implicit val rw: ReadWriter[ASTNumericNode] = ReadWriter.merge(ASTNumericConstant.rw)
}

case class ASTNumericConstant(value: Float) extends ASTNumericNode {
	def eval: Float = value
	override def nodeCount: Int = 0
	def getNthSubtree(subtree: Int): ASTNode = {
		this
	}

	def withInsertion(toInsert: ASTNode, insertionPoint: Int): ASTNode =
			this
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
	override def nodeCount: Int = 1 + cCons.nodeCount

	def getNthSubtree(subtree: Int): ASTNode = {
		if (subtree == 0)
			this
		else
			cCons.getNthSubtree(subtree - 1)
	}

	def withInsertion(toInsert: ASTNode, insertionPoint: Int): ASTNode =
		if (insertionPoint == 0)
			toInsert
		else if (insertionPoint < 0)
			this
		else
			this.copy(
				cCons = cCons.withInsertion(toInsert = toInsert, insertionPoint = insertionPoint - 1),
				valCons = valCons
			)
}

object ASTCapacitor {
	implicit val rw: ReadWriter[ASTCapacitor] = macroRW
}

case class ASTResistor(
	cCons: ASTNode,
	valCons: ASTNumericNode
) extends ASTNode {
	override def nodeCount: Int = 1 + cCons.nodeCount

	def getNthSubtree(subtree: Int): ASTNode = {
		if (subtree == 0)
			this
		else
			cCons.getNthSubtree(subtree - 1)
	}

	def withInsertion(toInsert: ASTNode, insertionPoint: Int): ASTNode =
		if (insertionPoint == 0)
			toInsert
		else if (insertionPoint < 0)
			this
		else
			this.copy(
				cCons = cCons.withInsertion(toInsert = toInsert, insertionPoint = insertionPoint - 1),
				valCons = valCons
			)
}

object ASTResistor {
	implicit val rw: ReadWriter[ASTResistor] = macroRW
}

case class ASTThreeGND(
	aCons: ASTNode,
	bCons: ASTNode,
	gndCons: ASTNode
) extends ASTNode {
	override def nodeCount: Int = 1 + aCons.nodeCount + bCons.nodeCount + gndCons.nodeCount

	def getNthSubtree(subtree: Int): ASTNode = {
		if (subtree == 0)
			this
		else if (subtree <= aCons.nodeCount)
			aCons.getNthSubtree(subtree - 1)
		else if (subtree <= aCons.nodeCount + bCons.nodeCount)
			bCons.getNthSubtree(subtree - 1 - aCons.nodeCount)
		else if (subtree <= aCons.nodeCount + bCons.nodeCount + gndCons.nodeCount)
		  gndCons.getNthSubtree(subtree - 1 - aCons.nodeCount - bCons.nodeCount)
		else {
			assert(false, "Got asked for a non-existent subtree")
			this
		}
	}

	def withInsertion(toInsert: ASTNode, insertionPoint: Int): ASTNode =
		if (insertionPoint == 0)
			toInsert
		else if (insertionPoint < 0)
			this
		else {
			this.copy(
				aCons = aCons.withInsertion(toInsert, insertionPoint - 1),
				bCons = bCons.withInsertion(toInsert, insertionPoint - aCons.nodeCount - 1),
				gndCons = gndCons.withInsertion(toInsert, insertionPoint - aCons.nodeCount - bCons.nodeCount - 1)
			)
		}
}

object ASTThreeGND {
	implicit val rw: ReadWriter[ASTThreeGND] = macroRW
}

case class ASTParallel(
	aCons: ASTNode,
	bCons: ASTNode
) extends ASTNode {
	override def nodeCount: Int = 1 + aCons.nodeCount + bCons.nodeCount

	def getNthSubtree(subtree: Int): ASTNode = {
		if (subtree == 0)
			this
		else if (subtree <= aCons.nodeCount)
			aCons.getNthSubtree(subtree - 1)
		else if (subtree <= aCons.nodeCount + bCons.nodeCount)
			bCons.getNthSubtree(subtree - 1 - aCons.nodeCount)
		else {
			assert(false, "Got asked for a non-existent subtree")
			this
		}
	}

	def withInsertion(toInsert: ASTNode, insertionPoint: Int): ASTNode =
		if (insertionPoint == 0)
			toInsert
		else if (insertionPoint < 0)
			this
		else {
			this.copy(
				aCons = aCons.withInsertion(toInsert, insertionPoint - 1),
				bCons = bCons.withInsertion(toInsert, insertionPoint - aCons.nodeCount - 1)
			)
		}
}

object ASTParallel {
	implicit val rw: ReadWriter[ASTParallel] = macroRW
}

case class ASTSeries(
	aCons: ASTNode,
	bCons: ASTNode
) extends ASTNode {
	override def nodeCount: Int = 1 + aCons.nodeCount + bCons.nodeCount

	def getNthSubtree(subtree: Int): ASTNode = {
		if (subtree == 0)
			this
		else if (subtree <= aCons.nodeCount)
			aCons.getNthSubtree(subtree - 1)
		else if (subtree <= aCons.nodeCount + bCons.nodeCount)
			bCons.getNthSubtree(subtree - 1 - aCons.nodeCount)
		else {
			assert(false, "Got asked for a non-existent subtree")
			this
		}
	}

	def withInsertion(toInsert: ASTNode, insertionPoint: Int): ASTNode =
		if (insertionPoint == 0)
			toInsert
		else if (insertionPoint < 0)
			this
		else
			this.copy(
				aCons = aCons.withInsertion(toInsert, insertionPoint - 1),
				bCons = bCons.withInsertion(toInsert, insertionPoint - aCons.nodeCount - 1)
			)
}

object ASTSeries {
	implicit val rw: ReadWriter[ASTSeries] = macroRW
}

class ASTEnd extends ASTNode {
	override def nodeCount: Int = 1

	def getNthSubtree(subtree: Int): ASTNode = {
		if (subtree == 0)
			this
		else {
			assert(false, "Got asked for a non-existent subtree")
			this
		}
	}

	def withInsertion(toInsert: ASTNode, insertionPoint: Int): ASTNode =
		if (insertionPoint == 0)
			toInsert
		else
			this

	override def toString: String = "ASTEnd"
}

object ASTEnd {
	def apply() = new ASTEnd
	implicit val rw: ReadWriter[ASTEnd] = macroRW
}

