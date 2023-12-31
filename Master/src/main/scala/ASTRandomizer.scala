object ASTRandomizer {
	private object Operation extends Enumeration {
		type Operation = Value
		val Resistor, Capacitor, Inductor, NPN, Parallel, Series, ThreeGND, Via0, Via1 = Value
	}
	private def randomNumericConstant(minValue: Float = 0, maxValue: Float = 1): Float = {
			scala.util.Random.nextFloat()*(maxValue-minValue) + minValue
	}
	def randomAST(maxDepth: Int): ASTNode = {
		if (maxDepth <= 1)
			new ASTEnd
		else {
			val operations = Seq(
				(10, Operation.Resistor),
				(10, Operation.Capacitor),
				(10, Operation.Inductor),
				(10, Operation.NPN),
				(10, Operation.Parallel),
				(10, Operation.Series),
				(10, Operation.ThreeGND),
				(10, Operation.Via0),
				(10, Operation.Via1))
			val totalWeight = operations.foldLeft(0)((accum, weightedOp) => accum + weightedOp._1)
			val selProb = scala.util.Random.nextFloat()*totalWeight

			var accumWeight = 0
			val selectedWeightedOp = operations.find(wop => {
				accumWeight = accumWeight+wop._1
				accumWeight > selProb}
			).get
 			createNode(selectedWeightedOp._2, maxDepth - 1)
		}
	}

	private def createNode(op: Operation.Operation, maxDepth: Int): ASTNode = {
		op match {
			case Operation.Resistor => ASTResistor(
				cCons = randomAST(maxDepth - 1),
				value = randomNumericConstant(minValue = 11, maxValue = 18)
			)
			case Operation.Capacitor => ASTCapacitor(
				cCons = randomAST(maxDepth - 1),
				value = randomNumericConstant(maxValue = 8)
			)
			case Operation.Inductor => ASTInductor(
				cCons = randomAST(maxDepth - 1),
				value = randomNumericConstant(minValue = 3, maxValue = 11)
			)
			case Operation.NPN => ASTNPN(
				collectorCons = randomAST(maxDepth - 1),
				baseCons = randomAST(maxDepth - 1),
				emitterCons = randomAST(maxDepth - 1)
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
			case Operation.Via0 => ASTVIA0(
				aCons = randomAST(maxDepth - 1),
				vCons = randomAST(maxDepth - 1)
			)
			case Operation.Via1 => ASTVIA1(
				aCons = randomAST(maxDepth - 1),
				vCons = randomAST(maxDepth - 1)
			)
		}

	}
}
