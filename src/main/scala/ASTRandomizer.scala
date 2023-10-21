object ASTRandomizer {
	object Operation extends Enumeration {
		type Operation = Value
		val Resistor, Capacitor, Parallel, Series, ThreeGND  = Value
}
	def randomNumericNode(minValue: Float = 0, maxValue: Float = 1): ASTNumericNode = {
		scala.util.Random.nextInt(1) match {
			case 0 => ASTNumericConstant(scala.util.Random.nextFloat()*(maxValue-minValue) + minValue)
		}
	}
	def randomAST(maxDepth: Int): ASTNode = {
		if (maxDepth <= 1)
			ASTEnd()
		else {
			val operations = Seq(
				(10, Operation.Resistor),
				(10, Operation.Capacitor),
				(10, Operation.Parallel),
				(10, Operation.Series),
				(5, Operation.ThreeGND))
			val totalWeight = operations.foldLeft(0)((accum, weightedOp) => accum + weightedOp._1)
			val selProb = scala.util.Random.nextFloat()*totalWeight

			var accumWeight = 0
			var selectedWeightedOp = operations.find(wop => {
				accumWeight = accumWeight+wop._1
				accumWeight > selProb}
			).get
 			createNode(selectedWeightedOp._2, maxDepth)
		}
	}

	private def createNode(op: Operation.Operation, maxDepth: Int): ASTNode = {
		op match {
			case Operation.Resistor => ASTResistor(
				cCons = randomAST(maxDepth - 1),
				valCons = randomNumericNode(minValue = 11, maxValue = 18)
			)
			case Operation.Capacitor => ASTCapacitor(
				cCons = randomAST(maxDepth - 1),
				valCons = randomNumericNode(maxValue = 8)
			)
			case Operation.Parallel => ASTParallel(
				aCons = randomAST(maxDepth - 1),
				bCons = randomAST(maxDepth - 1)
			)
			case Operation.Series => ASTSeries(
				aCons = randomAST(maxDepth - 1),
				bCons = randomAST(maxDepth - 1)
			)
			case Operation.ThreeGND => ASTThreeGND(
				aCons = randomAST(maxDepth - 1),
				bCons = randomAST(maxDepth - 1),
				gndCons = randomAST(maxDepth - 1)
			)
		}

	}
}
