object ASTCrossover {
	def crossover(treeA: ASTNode, treeB: ASTNode): Seq[ASTNode] = {
		val cutA = scala.util.Random.between(0, treeA.nodeCount)
		val cutB = scala.util.Random.between(0, treeB.nodeCount)
		val childA = cross(
			donor = treeA,
			cutPoint = cutA,
			receiver = treeB,
			insertionPoint = cutB)
		val childB = cross(
			donor = treeB,
			cutPoint = cutB,
			receiver = treeA,
			insertionPoint = cutA)
		Seq(childA, childB)
	}

	def cross(donor: ASTNode, cutPoint:Int, receiver: ASTNode, insertionPoint:Int): ASTNode = {
		val donatedSubtree = donor.getNthSubtree(cutPoint)
		receiver.withInsertion(donatedSubtree, insertionPoint)
	}

}
