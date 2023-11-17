import org.scalatest.funsuite.AnyFunSuiteLike

class ASTOperationsTest extends AnyFunSuiteLike {

	test("testSubtreeNull") {
		val node = ASTParallel(
			aCons = ASTCapacitor(cCons = ASTEnd(), value = 1),
			bCons = ASTCapacitor(cCons = ASTEnd(), value = 2)
		)
		val subTree = node.getNthSubtree(0)
		assert(node.toString == subTree.toString)
	}

	test("testSubtreeOne") {
		val node = ASTParallel(
			aCons = ASTCapacitor(cCons = ASTEnd(), value = 1),
			bCons = ASTCapacitor(cCons = ASTEnd(), value = 2)
		)
		val subTree = node.getNthSubtree(1)
		assert(node.aCons.toString == subTree.toString)
	}

	test("testSubtreeTwo") {
	  val leftTree = ASTCapacitor(cCons = ASTEnd(), value = 1)
		val node = ASTParallel(
			aCons = leftTree,
			bCons = ASTCapacitor(cCons = ASTEnd(), value = 2)
		)
		val subTree = node.getNthSubtree(2)
		assert(leftTree.cCons.toString == subTree.toString)
	}

	test("testSubtreeThree") {
		val leftTree = ASTCapacitor(cCons = ASTEnd(), value = 1)
		val rightTree = ASTResistor(cCons = ASTEnd(), value = 2)
		val node = ASTParallel(
			aCons = leftTree,
			bCons = rightTree
		)
		val subTree = node.getNthSubtree(3)
		assert(rightTree.toString == subTree.toString)
	}

	test("testSubtreeSeriesResistor") {
		val leftTree = ASTCapacitor(cCons = ASTEnd(), value = 1)
		val rightTree = ASTResistor(cCons = ASTEnd(), value = 2)
		val node = ASTSeries(
			aCons = leftTree,
			bCons = rightTree
		)
		val subTree = node.getNthSubtree(3)
		assert(rightTree.toString == subTree.toString)
	}

	test("testCrossEndResistor") {
		val leftTree = ASTEnd()
		val rightTree = ASTResistor(cCons = ASTEnd(), value = 2)
		val crossed = ASTOperations.cross(leftTree, 0, rightTree, 0)
		assert(crossed.toString == leftTree.toString)
	}

	test("testCrossCapacitorEnd") {
		val leftTree = ASTEnd()
		val rightTree = ASTCapacitor(
			cCons = ASTCapacitor(
				cCons = ASTEnd(),
				value = 44),
			value = 2)

		val crossed = ASTOperations.cross(donor = leftTree, 0, receiver = rightTree, 1)
		val expectedCross = ASTCapacitor(
			cCons = ASTEnd(),
			value = 2)
		assert(crossed.toJson.toString == expectedCross.toJson.toString)
	}

	test("testCrossResistorEnd") {
		val leftTree = ASTEnd()
		val rightTree = ASTResistor(
			cCons = ASTCapacitor(
				cCons = ASTEnd(),
				value = 44),
			value = 2)

		val crossed = ASTOperations.cross(donor = leftTree, 0, receiver = rightTree, 1)
		val expectedCross = ASTResistor(
			cCons = ASTEnd(),
			value = 2)
		assert(crossed.toJson.toString == expectedCross.toJson.toString)
	}

	test("testCrossInductorEnd") {
		val leftTree = ASTEnd()
		val rightTree = ASTInductor(
			cCons = ASTCapacitor(
				cCons = ASTEnd(),
				value = 44),
			value = 2)

		val crossed = ASTOperations.cross(donor = leftTree, 0, receiver = rightTree, 1)
		val expectedCross = ASTInductor(
			cCons = ASTEnd(),
			value = 2)
		assert(crossed.toJson.toString == expectedCross.toJson.toString)
	}

	test("testCrossSeriesParallel") {
		val leftTree1 = ASTCapacitor(cCons = ASTEnd(), value = 1)
		val rightTree1 = ASTResistor(cCons = ASTEnd(), value = 2)
		val node1 = ASTSeries(
			aCons = leftTree1,
			bCons = rightTree1
		)
		val leftTree2 = ASTCapacitor(cCons = ASTEnd(), value = 1)
		val rightTree2 = ASTResistor(cCons = ASTEnd(), value = 2)
		val node2 = ASTParallel(
			aCons = leftTree2,
			bCons = rightTree2
		)
		val crossed = ASTOperations.cross(donor = node1, 3, receiver = node2, 1)
		val expectedCross = ASTParallel(
			aCons = rightTree1,
			bCons = rightTree2
		)
		assert(crossed.toJson.toString == expectedCross.toJson.toString)
	}
}

