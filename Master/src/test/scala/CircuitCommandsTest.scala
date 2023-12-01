import org.scalatest.BeforeAndAfter

class CircuitCommandsTest extends org.scalatest.funsuite.AnyFunSuiteLike with BeforeAndAfter {
	before {
		CircuitResistor.reset()
		CircuitCapacitor.reset()
		CircuitInductor.reset()
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
		assert(circuit.toUndecoratedSpice == "L2 A B 100uH")
	}

	test(testName = "Circuit.oneASTEndJSON") {
		assert(ASTEnd().toJson.toString() == "{\"type\":\"End\",\"constructors\":[]}")
	}
	test(testName = "Circuit.oneASTParallelJSON") {
		assert(ASTParallel(aCons = ASTEnd(), bCons = ASTEnd()).toJson.toString() == "{\"type\":\"Parallel\",\"constructors\":[{\"type\":\"End\",\"constructors\":[]},{\"type\":\"End\",\"constructors\":[]}]}")
	}

	test(testName = "Circuit.oneASTCapacitorJSON") {
		assert(ASTCapacitor(cCons = ASTEnd(), value = 7).toJson.toString() == "{\"type\":\"Capacitor\",\"constructors\":[{\"type\":\"End\",\"constructors\":[]}],\"value\":\"7.0\"}")
	}

	test(testName = "Circuit.oneASTResistorJSON") {
		assert(ASTResistor(cCons = ASTEnd(), value = 7).toJson.toString() == "{\"type\":\"Resistor\",\"constructors\":[{\"type\":\"End\",\"constructors\":[]}],\"value\":\"7.0\"}")
	}


	test(testName = "Circuit.oneASTInductorJSON") {
		assert(ASTInductor(cCons = ASTEnd(), value = 7).toJson.toString() == "{\"type\":\"Inductor\",\"constructors\":[{\"type\":\"End\",\"constructors\":[]}],\"value\":\"7.0\"}")
	}

	test(testName = "Circuit.oneResistorCircuitToJSON") {
		val externalNodes = Seq(CircuitNode("A"), CircuitNode("B"))
		val w1 = CircuitWire(nodes = externalNodes)
		val circuit = CircuitBuilder.applyCommand(w1, ASTResistor(cCons = ASTEnd(), value = 7)).cleanCircuit(externalNodes)
		assert(circuit.toJson.toString == "{\"nodes\":[{\"name\":\"A\"},{\"name\":\"B\"}],\"components\":[{\"type\":\"Resistor\",\"nodes\":[{\"name\":\"A\"},{\"name\":\"B\"}],\"value\":\"7.0\",\"ident\":2}]}")
	}

	test(testName = "Circuit.oneInductorCircuitToJSON") {
		val externalNodes = Seq(CircuitNode("A"), CircuitNode("B"))
		val w1 = CircuitWire(nodes = externalNodes)
		val circuit = CircuitBuilder.applyCommand(w1, ASTInductor(cCons = ASTEnd(), value = 7)).cleanCircuit(externalNodes)
		CircuitInductor.reset()
		assert(circuit.components == Seq(CircuitInductor(nodes = externalNodes, value = 7, inductorNumber = 2)))
		assert(circuit.toJson.toString() == "{\"nodes\":[{\"name\":\"A\"},{\"name\":\"B\"}],\"components\":[{\"type\":\"Inductor\",\"nodes\":[{\"name\":\"A\"},{\"name\":\"B\"}],\"value\":\"7.0\",\"ident\":2}]}")
	}}
