import org.scalatest.BeforeAndAfter

class CircuitCommandsTest extends org.scalatest.funsuite.AnyFunSuiteLike with BeforeAndAfter {
	before {
		CircuitResistor.reset()
		CircuitCapacitor.reset()
		CircuitInductor.reset()
		Circuit.reset()
	}

	test(testName = "Circuit.toSpiceOneResistor") {
		val testResistor = CircuitResistor(nodes = Seq(CircuitNode("A"), CircuitNode("B")), 11)
		assert(testResistor.toSpice == "R1 A B 1")
	}

	test(testName = "Circuit.twoResistors") {
		val r1 = CircuitResistor(nodes = Seq(CircuitNode("A"), CircuitNode("B")), 12)
		val r2 = CircuitResistor(nodes = Seq(CircuitNode("A"), CircuitNode("B")), 12)
		assert(r1.toSpice == "R1 A B 10")
		assert(r2.toSpice == "R2 A B 10")
	}

	test(testName = "Circuit.parallelResistors") {
		val r1 = CircuitResistor(nodes = Seq(CircuitNode("A"), CircuitNode("B")), 13)
		val command = ASTParallel(aCons = ASTEnd(), bCons = ASTEnd())
		val circuit = CircuitBuilder.applyCommand(r1, command)
		assert(circuit.toUndecoratedSpice == "R1 A B 100\nR2 A B 100")
	}

	test(testName = "Circuit.seriesResistors") {
		val r1 = CircuitResistor(nodes = Seq(CircuitNode("A"), CircuitNode("B")), 11)
		val command = ASTSeries(aCons = ASTEnd(), bCons = ASTEnd())
		val circuit = CircuitBuilder.applyCommand(r1, command)
		assert(circuit.toUndecoratedSpice == "R2 A AB 1\nR3 AB B 1")
	}

	test(testName = "Circuit.fromWireToResistor") {
		val w1 = CircuitWire(nodes = Seq(CircuitNode("A"), CircuitNode("B")))
		val command = ASTResistor(cCons = ASTEnd(), value = 11)
		val circuit = CircuitBuilder.applyCommand(w1, command)
		assert(circuit.toUndecoratedSpice == "R1 A B 1")
	}

	test(testName = "Circuit.fromWireToParallelResistors") {
		val w1 = CircuitWire(nodes = Seq(CircuitNode("A"), CircuitNode("B")))
		val command = ASTParallel(
			aCons = ASTResistor(cCons = ASTEnd(), value = 11),
			bCons = ASTResistor(cCons = ASTEnd(), value = 13))
		val circuit = CircuitBuilder.applyCommand(w1, command)
		assert(circuit.toUndecoratedSpice == "R1 A B 1\nR2 A B 100")
	}

	test(testName = "Circuit.fromWireToParallelCapacitors") {
		val w1 = CircuitWire(nodes = Seq(CircuitNode("A"), CircuitNode("B")))
		val command = ASTParallel(
			aCons = ASTCapacitor(cCons = ASTEnd(), value = 1),
			bCons = ASTCapacitor(cCons = ASTEnd(), value = 2))
		val circuit = CircuitBuilder.applyCommand(w1, command)
		assert(circuit.toUndecoratedSpice == "C1 A B 100pF\nC2 A B 1nF")
	}

	test(testName = "Circuit.fromCapacitorToResistor") {
		val c1 = CircuitCapacitor(nodes = Seq(CircuitNode("A"), CircuitNode("B")), 23)
		val command = ASTResistor(cCons = ASTEnd(), value = 14)
		val circuit = CircuitBuilder.applyCommand(c1, command)
		assert(circuit.toUndecoratedSpice == "R1 A B 1k")
	}

	test(testName = "Circuit.fromResistorToCapacitor") {
		val r1 = CircuitResistor(nodes = Seq(CircuitNode("A"), CircuitNode("B")), 23)
		val command = ASTCapacitor(cCons = ASTEnd(), value = 2)
		val circuit = CircuitBuilder.applyCommand(r1, command)
		assert(circuit.toUndecoratedSpice == "C1 A B 1nF")
	}

	test(testName = "Circuit.fromWireToEmpty") {
		val externalNodes = Seq(CircuitNode("A"), CircuitNode("B"))
		val w1 = CircuitWire(nodes = externalNodes)
		val circuit = CircuitBuilder.applyCommand(w1, ASTEnd()).cleanCircuit(externalNodes)
		assert(circuit.toUndecoratedSpice == "")
	}

	test(testName = "Circuit.fromWireToInductor") {
		val externalNodes = Seq(CircuitNode("A"), CircuitNode("B"))
		val w1 = CircuitWire(nodes = externalNodes)
		val circuit = CircuitBuilder.applyCommand(w1, ASTInductor(cCons = ASTEnd(), value = 7)).cleanCircuit(externalNodes)
		assert(circuit.toUndecoratedSpice == "L1 A B 100uH")
	}

	test(testName = "Circuit.oneASTInductorJSON") {
		assert(upickle.default.write(ASTInductor(cCons = ASTEnd(), value = 7)) == "{\"$type\":\"ASTInductor\",\"cCons\":{\"$type\":\"ASTEnd\"},\"value\":7}")
	}

	test(testName = "Circuit.oneResistorCircuitToJSON") {
		val externalNodes = Seq(CircuitNode("A"), CircuitNode("B"))
		val w1 = CircuitWire(nodes = externalNodes)
		val circuit = CircuitBuilder.applyCommand(w1, ASTResistor(cCons = ASTEnd(), value = 7)).cleanCircuit(externalNodes)
		assert(upickle.default.write(circuit) == "{\"nodes\":[{\"name\":\"A\"},{\"name\":\"B\"}],\"components\":[{\"$type\":\"CircuitResistor\",\"nodes\":[{\"name\":\"A\"},{\"name\":\"B\"}],\"value\":7,\"resistorNumber\":1}],\"circuitNumber\":1}")
	}

	test(testName = "Circuit.oneInductorCircuitToJSON") {
		val externalNodes = Seq(CircuitNode("A"), CircuitNode("B"))
		val w1 = CircuitWire(nodes = externalNodes)
		val circuit = CircuitBuilder.applyCommand(w1, ASTInductor(cCons = ASTEnd(), value = 7)).cleanCircuit(externalNodes)
		assert(upickle.default.write(circuit) == "{\"nodes\":[{\"name\":\"A\"},{\"name\":\"B\"}],\"components\":[{\"$type\":\"CircuitInductor\",\"nodes\":[{\"name\":\"A\"},{\"name\":\"B\"}],\"value\":7,\"inductorNumber\":1}],\"circuitNumber\":1}")
	}}
