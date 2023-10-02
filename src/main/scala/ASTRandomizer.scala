object ASTRandomizer {
	def randomNumericNode: ASTNumericNode = {
		scala.util.Random.nextInt(1) match {
			case 0 => ASTNumericConstant(scala.util.Random.nextFloat())
		}
	}
	def randomAST(maxDepth: Int): ASTNode = {
		if (maxDepth <= 1)
			ASTEnd
		else {
			scala.util.Random.nextInt(4) match {
				case 0 => ASTResistor(
					cCons = randomAST(maxDepth - 1),
					valCons = randomNumericNode
				)
				case 1 => ASTCapacitor(
					cCons = randomAST(maxDepth - 1),
					valCons = randomNumericNode
				)
				case 2 => ASTParallel(
					cConsA = randomAST(maxDepth - 1),
					cConsB = randomAST(maxDepth - 1)
				)
				case 3 => ASTSeries(
					cConsA = randomAST(maxDepth - 1),
					cConsB = randomAST(maxDepth - 1)
				)
			}
		}
	}
}
