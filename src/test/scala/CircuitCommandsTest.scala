import org.scalatest.BeforeAndAfter

class CircuitCommandsTest extends org.scalatest.funsuite.AnyFunSuiteLike with BeforeAndAfter {
	before {
		CircuitResistor.reset()
		CircuitCapacitor.reset()
	}
	test(testName = "Circuit.toSpiceOneResistor") {
		val testResistor = CircuitResistor(nodes = Seq(CircuitNode("A"), CircuitNode("B")), 2.0f)
		assert(testResistor.toSpice == "R1 A B 2.0")
	}
	test(testName = "Circuit.twoResistors") {
		val r1 = CircuitResistor(nodes = Seq(CircuitNode("A"), CircuitNode("B")), 2.0f)
		val r2 = CircuitResistor(nodes = Seq(CircuitNode("A"), CircuitNode("B")), 2.0f)
		assert(r1.toSpice == "R1 A B 2.0")
		assert(r2.toSpice == "R2 A B 2.0")
	}
	test(testName = "Circuit.parallelResistors") {
		val r1 = CircuitResistor(nodes = Seq(CircuitNode("A"), CircuitNode("B")), 2.0f)
		val command = ASTParallel(cConsA = ASTEnd, cConsB = ASTEnd)
		val circuit = r1.applyCommand(command)
		assert(circuit.toSpice == "R1 A B 2.0\nR2 A B 2.0")
	}
	test(testName = "Circuit.seriesResistors") {
		val r1 = CircuitResistor(nodes = Seq(CircuitNode("A"), CircuitNode("B")), 2.0f)
		val command = ASTSeries(cConsA = ASTEnd, cConsB = ASTEnd)
		val circuit = r1.applyCommand(command)
		assert(circuit.toSpice == "R2 A AB 2.0\nR3 AB B 2.0")
	}
	test(testName = "Circuit.fromWireToResistor") {
		val w1 = CircuitWire(nodes = Seq(CircuitNode("A"), CircuitNode("B")))
		val command = ASTResistor(cCons = ASTEnd, valCons = ASTNumericConstant(2.0f))
		val circuit = w1.applyCommand(command)
		assert(circuit.toSpice == "R1 A B 2.0")
	}
	test(testName = "Circuit.fromWireToParallelResistors") {
		val w1 = CircuitWire(nodes = Seq(CircuitNode("A"), CircuitNode("B")))
		val command = ASTParallel(
			cConsA = ASTResistor(cCons = ASTEnd, valCons = ASTNumericConstant(2.0f)),
			cConsB = ASTResistor(cCons = ASTEnd, valCons = ASTNumericConstant(22.0f)))
		val circuit = w1.applyCommand(command)
		assert(circuit.toSpice == "R1 A B 2.0\nR2 A B 22.0")
	}
	test(testName = "Circuit.fromWireToParallelCapacitors") {
		val w1 = CircuitWire(nodes = Seq(CircuitNode("A"), CircuitNode("B")))
		val command = ASTParallel(
			cConsA = ASTCapacitor(cCons = ASTEnd, valCons = ASTNumericConstant(2.0f)),
			cConsB = ASTCapacitor(cCons = ASTEnd, valCons = ASTNumericConstant(22.0f)))
		val circuit = w1.applyCommand(command)
		assert(circuit.toSpice == "C1 A B 2.0\nC2 A B 22.0")
	}
	test(testName = "Circuit.fromCapacitorToResistor") {
		val c1 = CircuitCapacitor(nodes = Seq(CircuitNode("A"), CircuitNode("B")), 23)
		val command = ASTResistor(cCons = ASTEnd, valCons = ASTNumericConstant(2.0f))
		val circuit = c1.applyCommand(command)
		assert(circuit.toSpice == "R1 A B 2.0")
	}
	test(testName = "Circuit.fromResistorToCapacitor") {
		val r1 = CircuitResistor(nodes = Seq(CircuitNode("A"), CircuitNode("B")), 23)
		val command = ASTCapacitor(cCons = ASTEnd, valCons = ASTNumericConstant(2.0f))
		val circuit = r1.applyCommand(command)
		assert(circuit.toSpice == "C1 A B 2.0")
	}
	test(testName = "Circuit.fromWireToEmpty") {
		val externalNodes = Seq(CircuitNode("A"), CircuitNode("B"))
		val w1 = CircuitWire(nodes = externalNodes)
		val command = ASTEnd
		val circuit = w1.applyCommand(command).cleanCircuit(externalNodes)
		assert(circuit.toSpice == "")
	}
}
