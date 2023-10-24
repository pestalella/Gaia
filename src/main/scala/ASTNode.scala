import upickle.default._

sealed trait ASTNode {
	def nodeCount: Int

	def height: Int

	def getNthSubtree(subtree: Int): ASTNode

	def withInsertion(toInsert: ASTNode, insertionPoint: Int): ASTNode

	def mutate(mutationPoint: Int): ASTNode

	def coinFlip(trueProb: Float): Boolean = {
		scala.util.Random.nextFloat() <= trueProb
	}
}
object ASTNode{
  implicit val rw: ReadWriter[ASTNode] = ReadWriter.merge(
		ASTCapacitor.rw,
		ASTResistor.rw,
		ASTInductor.rw,
		ASTParallel.rw,
		ASTSeries.rw,
		ASTThreeGND.rw,
		ASTEnd.rw
	)
}

case class ASTCapacitor(
	cCons: ASTNode,
	value: Float
) extends ASTNode {
	override def nodeCount: Int = 1 + cCons.nodeCount
	override def height: Int = 1 + cCons.height

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
				value = value
			)

	def mutate(mutationPoint: Int): ASTNode = {
		if (mutationPoint == 0) {
			// Either return a whole new tree or this same node slightly mutated in value
			if (coinFlip(Parameters.mutationRate)) {
				ASTRandomizer.randomAST(height)
			} else {
				this.copy(value = value + (value * (0.2f * scala.util.Random.nextFloat() - 0.1f)))
			}
		} else {
			// This is not the point we wanted to mutate
			if (mutationPoint - 1 <= cCons.nodeCount)
				this.copy(
					cCons = cCons.mutate(mutationPoint - 1),
					value = value
				)
			else
				this
		}
	}
}

object ASTCapacitor {
	implicit val rw: ReadWriter[ASTCapacitor] = macroRW
}

case class ASTResistor(
	cCons: ASTNode,
	value: Float
) extends ASTNode {
	override def nodeCount: Int = 1 + cCons.nodeCount
	override def height: Int = 1 + cCons.height

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
				value = value
			)

	def mutate(mutationPoint: Int): ASTNode = {
		if (mutationPoint == 0) {
			// Either return a whole new tree or this same node slightly mutated in value
			if (coinFlip(Parameters.mutationRate)) {
				ASTRandomizer.randomAST(height)
			} else {
				this.copy(value = value + (value * (0.2f * scala.util.Random.nextFloat() - 0.1f)))
			}
		} else {
			// This is not the point we wanted to mutate
			if (mutationPoint - 1 <= cCons.nodeCount)
				this.copy(
					cCons = cCons.mutate(mutationPoint - 1),
					value = value
				)
			else
				this
		}
	}
}

object ASTResistor {
	implicit val rw: ReadWriter[ASTResistor] = macroRW
}

case class ASTInductor(
	cCons: ASTNode,
	value: Float
) extends ASTNode {
	override def nodeCount: Int = 1 + cCons.nodeCount
	override def height: Int = 1 + cCons.height

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
				value = value
			)

	def mutate(mutationPoint: Int): ASTNode = {
		if (mutationPoint == 0) {
			// Either return a whole new tree or this same node slightly mutated in value
			if (coinFlip(Parameters.mutationRate)) {
				ASTRandomizer.randomAST(height)
			} else {
				this.copy(value = value + (value * (0.2f * scala.util.Random.nextFloat() - 0.1f)))
			}
		} else {
			// This is not the point we wanted to mutate
			if (mutationPoint - 1 <= cCons.nodeCount)
				this.copy(
					cCons = cCons.mutate(mutationPoint - 1),
					value = value
				)
			else
				this
		}
	}
}

object ASTInductor {
	implicit val rw: ReadWriter[ASTInductor] = macroRW
}

case class ASTThreeGND(
	aCons: ASTNode,
	bCons: ASTNode,
	gndCons: ASTNode
) extends ASTNode {
	override def nodeCount: Int = 1 + aCons.nodeCount + bCons.nodeCount + gndCons.nodeCount
	override def height: Int = 1 + scala.math.max(scala.math.max(aCons.height, bCons.height), gndCons.height)

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
			assert(assertion = false, "Got asked for a non-existent subtree")
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

	def mutate(mutationPoint: Int): ASTNode = {
		if (mutationPoint == 0) {
			// Either return a whole new tree or this same node
			if (coinFlip(Parameters.mutationRate)) {
				ASTRandomizer.randomAST(height)
			} else {
				this
			}
		} else {
			// This is not the point we wanted to mutate
			if (mutationPoint - 1 <= aCons.nodeCount)
				this.copy(
					aCons = aCons.mutate(mutationPoint - 1)
				)
			else if (mutationPoint - 1 <= aCons.nodeCount + bCons.nodeCount)
				this.copy(
					bCons = bCons.mutate(mutationPoint - aCons.nodeCount - 1)
				)
			else if (mutationPoint - 1 <= aCons.nodeCount + bCons.nodeCount + gndCons.nodeCount)
				this.copy(
					gndCons = gndCons.mutate(mutationPoint - aCons.nodeCount - bCons.nodeCount - 1)
				)
			else
				this
		}
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
	override def height: Int = 1 + scala.math.max(aCons.height, bCons.height)

	def getNthSubtree(subtree: Int): ASTNode = {
		if (subtree == 0)
			this
		else if (subtree <= aCons.nodeCount)
			aCons.getNthSubtree(subtree - 1)
		else if (subtree <= aCons.nodeCount + bCons.nodeCount)
			bCons.getNthSubtree(subtree - 1 - aCons.nodeCount)
		else {
			assert(assertion = false, "Got asked for a non-existent subtree")
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

	def mutate(mutationPoint: Int): ASTNode = {
		if (mutationPoint == 0) {
			// Either return a whole new tree or this same node
			if (coinFlip(Parameters.mutationRate)) {
				ASTRandomizer.randomAST(height)
			} else {
				this
			}
		} else {
			// This is not the point we wanted to mutate, go deeper
			if (mutationPoint - 1 <= aCons.nodeCount)
				this.copy(
					aCons = aCons.mutate(mutationPoint - 1)
				)
			else if (mutationPoint - 1 <= aCons.nodeCount + bCons.nodeCount)
				this.copy(
					bCons = bCons.mutate(mutationPoint - aCons.nodeCount - 1)
				)
			else {
				// Nope, the mutation point is outside this subtree
				this
			}
		}
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
	override def height: Int = 1 + scala.math.max(aCons.height, bCons.height)

	def getNthSubtree(subtree: Int): ASTNode = {
		if (subtree == 0)
			this
		else if (subtree <= aCons.nodeCount)
			aCons.getNthSubtree(subtree - 1)
		else if (subtree <= aCons.nodeCount + bCons.nodeCount)
			bCons.getNthSubtree(subtree - 1 - aCons.nodeCount)
		else {
			assert(assertion = false, "Got asked for a non-existent subtree")
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

	def mutate(mutationPoint: Int): ASTNode = {
		if (mutationPoint == 0) {
			// Either return a whole new tree or this same node
			if (coinFlip(Parameters.mutationRate)) {
				ASTRandomizer.randomAST(height)
			} else {
				this
			}
		} else {
			// This is not the point we wanted to mutate
			if (mutationPoint - 1 <= aCons.nodeCount)
				this.copy(
					aCons = aCons.mutate(mutationPoint - 1)
				)
			else if (mutationPoint - 1 <= aCons.nodeCount + bCons.nodeCount)
				this.copy(
					bCons = bCons.mutate(mutationPoint - aCons.nodeCount - 1)
				)
			else
				this
		}
	}
}

object ASTSeries {
	implicit val rw: ReadWriter[ASTSeries] = macroRW
}

class ASTEnd extends ASTNode {
	override def nodeCount: Int = 1
	override def height: Int = 1

	def getNthSubtree(subtree: Int): ASTNode = {
		if (subtree == 0)
			this
		else {
			assert(assertion = false, "Got asked for a non-existent subtree")
			this
		}
	}

	def withInsertion(toInsert: ASTNode, insertionPoint: Int): ASTNode =
		if (insertionPoint == 0)
			toInsert
		else
			this

	override def toString: String = "ASTEnd"

	def mutate(mutationPoint: Int): ASTNode = {
		if (mutationPoint == 0) {
			if (coinFlip(Parameters.mutationRate)) {
				// Either return a whole new tree
				ASTRandomizer.randomAST(height + 1)
			} else {
				// Or return this same node
				this
			}
		} else
				this
		}
}

object ASTEnd {
	def apply() = new ASTEnd
	implicit val rw: ReadWriter[ASTEnd] = macroRW
}

