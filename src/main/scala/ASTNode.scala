 object ASTNodeType extends Enumeration {
	type ASTNodeType = Value
	val Resistor, Capacitor, Inductor, Parallel, Series, ThreeGND, Via0, End = Value
}

abstract class ASTNode(
	val nodeType: ASTNodeType.ASTNodeType,
	val constructors: Seq[ASTNode]
) {
	def nodeCount: Int = 1 + constructors.foldLeft(0)((accum, node) => accum + node.nodeCount)

	def height: Int = 1 + constructors.foldLeft(0)((maxHeight, node) => scala.math.max(maxHeight, node.height))

	def getNthSubtree(subtree: Int): ASTNode = {
		if (subtree == 0)
			this
		else {
			var nodeCount = 0
			val nodeWithSubtree = constructors.find(cons => {
				nodeCount += cons.nodeCount
				subtree <= nodeCount
			})
			nodeWithSubtree match {
				case None =>
					assert(assertion = false, "Got asked for a non-existent subtree")
					this
				case Some(node) => node.getNthSubtree(subtree - (nodeCount - node.nodeCount) - 1)
			}
		}
	}
	def withInsertion(toInsert: ASTNode, insertionPoint: Int): ASTNode =
		if (insertionPoint == 0)
			toInsert
		else if (insertionPoint < 0)
			this
		else {
			var nodeCount = 0
			val newCons = constructors map (cons => {
				nodeCount += cons.nodeCount
				if (insertionPoint - 1 <= nodeCount)
					cons.withInsertion(toInsert, insertionPoint - 1 - (nodeCount - cons.nodeCount))
				else
					cons
			})
			this.copy(
				constructors = newCons
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
			var nodeCount = 0
			val mutatedConstructors = constructors.map(cons => {
				nodeCount += cons.nodeCount
				if (mutationPoint - 1 <= nodeCount)
					cons.mutate(mutationPoint - 1 - (nodeCount - cons.nodeCount))
				else
					cons
			})
			this.copy(
				constructors = mutatedConstructors
			)
		}
	}

	def coinFlip(trueProb: Float): Boolean = {
		scala.util.Random.nextFloat() <= trueProb
	}

	def copy(constructors: Seq[ASTNode]): ASTNode

	def toJson: ujson.Obj = {
		ujson.Obj(
			"type" -> nodeType.toString,
			"constructors" -> ujson.Arr.from(constructors map (_.toJson))
		)
	}
}

abstract class ASTNodeWithValue(
	nodeValueType: ASTNodeType.ASTNodeType,
	nvCons: ASTNode,
	nValue: Float
) extends ASTNode(nodeType = nodeValueType, constructors = Seq(nvCons)) {
	override def withInsertion(toInsert: ASTNode, insertionPoint: Int): ASTNode =
		if (insertionPoint == 0)
			toInsert
		else if (insertionPoint < 0)
			this
		else {
			this.copy(
				constructors = Seq(nvCons.withInsertion(toInsert, insertionPoint - 1))
			)
		}

	override def toJson: ujson.Obj = {
		ujson.Obj(
			"type" -> nodeType.toString,
			"constructors" -> ujson.Arr.from(constructors map (_.toJson)),
			"value" -> nValue.toString
		)
	}
}

case class ASTCapacitor(
	cCons: ASTNode,
	value: Float
) extends ASTNodeWithValue(
	nodeValueType = ASTNodeType.Capacitor,
	nvCons = cCons,
	nValue = value) {
	override def mutate(mutationPoint: Int): ASTNode = {
		if (mutationPoint == 0) {
			// Either return a whole new tree or this same node slightly mutated in value
			if (coinFlip(Parameters.mutationRate)) {
				ASTRandomizer.randomAST(height)
			} else {
				ASTCapacitor(cCons = cCons, value = value + (value * (0.2f * scala.util.Random.nextFloat() - 0.1f)))
			}
		} else {
			// This is not the point we wanted to mutate
			if (mutationPoint - 1 <= cCons.nodeCount)
				ASTCapacitor(
					cCons = cCons.mutate(mutationPoint - 1),
					value = value
				)
			else
				this
		}
	}

	override def copy(constructors: Seq[ASTNode] = Seq(cCons)): ASTNode = ASTCapacitor(
		cCons = constructors.head,
		value = value
	)
}

case class ASTResistor(
	cCons: ASTNode,
	value: Float
) extends ASTNodeWithValue(
	nodeValueType = ASTNodeType.Resistor,
	nvCons = cCons,
	nValue = value) {
	override def mutate(mutationPoint: Int): ASTNode = {
		if (mutationPoint == 0) {
			// Either return a whole new tree or this same node slightly mutated in value
			if (coinFlip(Parameters.mutationRate)) {
				ASTRandomizer.randomAST(height)
			} else {
				ASTResistor(cCons = cCons, value = value + (value * (0.2f * scala.util.Random.nextFloat() - 0.1f)))
			}
		} else {
			// This is not the point we wanted to mutate
			if (mutationPoint - 1 <= cCons.nodeCount)
				ASTResistor(
					cCons = cCons.mutate(mutationPoint - 1),
					value = value
				)
			else
				this
		}
	}

	override def copy(constructors: Seq[ASTNode] = Seq(cCons)): ASTNode = ASTResistor(
		cCons = constructors.head,
		value = value
	)
}

case class ASTInductor(
	cCons: ASTNode,
	value: Float
) extends ASTNodeWithValue(
	nodeValueType = ASTNodeType.Inductor,
	nvCons = cCons,
	nValue = value) {
	override def mutate(mutationPoint: Int): ASTNode = {
		if (mutationPoint == 0) {
			// Either return a whole new tree or this same node slightly mutated in value
			if (coinFlip(Parameters.mutationRate)) {
				ASTRandomizer.randomAST(height)
			} else {
				ASTInductor(cCons = cCons, value = value + (value * (0.2f * scala.util.Random.nextFloat() - 0.1f)))
			}
		} else {
			// This is not the point we wanted to mutate
			if (mutationPoint - 1 <= cCons.nodeCount)
				ASTInductor(
					cCons = cCons.mutate(mutationPoint - 1),
					value = value
				)
			else
				this
		}
	}

	override def copy(constructors: Seq[ASTNode] = Seq(cCons)): ASTNode = ASTInductor(
		cCons = constructors.head,
		value = value
	)
}

case class ASTThreeGND(
	aCons: ASTNode,
	bCons: ASTNode,
	gndCons: ASTNode
) extends ASTNode(nodeType = ASTNodeType.ThreeGND, constructors = Seq(aCons, bCons, gndCons)) {

	override def copy(constructors: Seq[ASTNode] = Seq(aCons, bCons, gndCons)): ASTNode = ASTThreeGND(
		aCons = constructors(0),
		bCons = constructors(1),
		gndCons = constructors(2)
	)
}

case class ASTParallel(
	aCons: ASTNode,
	bCons: ASTNode
) extends ASTNode(nodeType = ASTNodeType.Parallel, constructors = Seq(aCons, bCons)) {
	override def copy(constructors: Seq[ASTNode] = Seq(aCons, bCons)): ASTNode = ASTParallel(
		aCons = constructors(0),
		bCons = constructors(1)
	)
}

case class ASTSeries(
	aCons: ASTNode,
	bCons: ASTNode
) extends ASTNode(nodeType = ASTNodeType.Series, constructors = Seq(aCons, bCons)) {
	override def copy(constructors: Seq[ASTNode] = Seq(aCons, bCons)): ASTNode = ASTSeries(
		aCons = constructors(0),
		bCons = constructors(1)
	)
}

case class ASTVIA0(
	aCons: ASTNode,
	vCons: ASTNode
) extends ASTNode(nodeType = ASTNodeType.Via0, constructors = Seq(aCons,  vCons)) {
	override def copy(constructors: Seq[ASTNode] = Seq(aCons, vCons)): ASTNode = ASTVIA0(
		aCons = constructors(0),
		vCons = constructors(1)
	)
}

 class ASTEnd extends ASTNode(nodeType = ASTNodeType.End, constructors = Seq()) {
	 override def toString: String = "ASTEnd"

	 override def copy(constructors: Seq[ASTNode]): ASTNode = ASTEnd()
 }

 object ASTEnd {
	 def apply(): ASTEnd = new ASTEnd
 }
