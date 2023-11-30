import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.BeforeAndAfter

class CircuitTest extends AnyFunSuiteLike with BeforeAndAfter {
	before {
		CircuitResistor.reset()
		CircuitCapacitor.reset()
		CircuitInductor.reset()
	}

	test("testEnumerateNodesSingle") {
		val c = Circuit(nodes = Seq(CircuitNode("AAA")), components = Seq())
		val cc = c.enumerateNodes(Seq())
		assert(cc.nodes == Seq(CircuitNode("1")))
	}
	test("testEnumerateNodesDouble") {
		val c = Circuit(nodes = Seq(CircuitNode("AAA"), CircuitNode("DD")), components = Seq())
		val cc = c.enumerateNodes(Seq())
		assert(cc.nodes == Seq(CircuitNode("1"), CircuitNode("2")))
	}
	test("testCleanCircuit") {
		val c = Circuit(nodes = Seq(CircuitNode("A"), CircuitNode("AAA"), CircuitNode("B")), components = Seq())
		val cc = c.cleanCircuit(Seq(CircuitNode("A"), CircuitNode("B")))
		assert(cc.nodes == Seq(CircuitNode("A"), CircuitNode("1"), CircuitNode("B")))
	}
	test("testCleanCircuitWithComponents") {
		val c = Circuit(
			nodes = Seq(CircuitNode("A"), CircuitNode("AAA"), CircuitNode("B")),
			components = Seq(CircuitResistor(nodes = Seq(CircuitNode("AAA"), CircuitNode("B")), value = 6)))
		val cc = c.cleanCircuit(Seq(CircuitNode("A"), CircuitNode("B")))
		assert(cc.nodes == Seq(CircuitNode("A"), CircuitNode("1"), CircuitNode("B")))
	}
	test("testCleanRealCircuit") {
		val c = Circuit(
			nodes = Seq(CircuitNode("A"), CircuitNode("A0"), CircuitNode("0"), CircuitNode("A0VIA1"), CircuitNode("VIA1")),
			components = Seq(
				CircuitResistor(nodes = Seq(CircuitNode("A"), CircuitNode("A0")), value = 14),
				CircuitCapacitor(nodes = Seq(CircuitNode("A0"), CircuitNode("0")), value = 3),
				CircuitWire(nodes = Seq(CircuitNode("A0"), CircuitNode("A0VIA1"))),
				CircuitWire(nodes = Seq(CircuitNode("A0VIA1"), CircuitNode("VIA1")))
			)
		)
		val cc = c.cleanCircuit(Seq(CircuitNode("A"), CircuitNode("B"), CircuitNode("0")))
		assert(cc.nodes == Seq(CircuitNode("A"), CircuitNode("0"), CircuitNode("1")))
	}
	test("testCleanCircuitToSpice") {
		val c = Circuit(
			nodes = Seq(CircuitNode("A"), CircuitNode("AAA"), CircuitNode("B")),
			components = Seq(CircuitResistor(nodes = Seq(CircuitNode("AAA"), CircuitNode("B")), value = 12)))
		val cc = c.cleanCircuit(Seq(CircuitNode("A"), CircuitNode("B")))
		assert(cc.nodes == Seq(CircuitNode("A"), CircuitNode("1"), CircuitNode("B")))
		assert(cc.toUndecoratedSpice == "R2 1 B 10")
	}

	test("testCleanCircuitResistorAtoB") {
		val c = Circuit(
			nodes = Seq(CircuitNode("A"), CircuitNode("AAA"), CircuitNode("B")),
			components = Seq(CircuitResistor(nodes = Seq(CircuitNode("A"), CircuitNode("B")), value = 6)))
		val cc = c.cleanCircuit(Seq(CircuitNode("A"), CircuitNode("B")))
		assert(cc.components == Seq(CircuitResistor(nodes = Seq(CircuitNode("A"), CircuitNode("B")), value = 6, resistorNumber = 2)))
	}

	test("testCleanCircuitTwoResistors") {
		val c = Circuit(
			nodes = Seq(CircuitNode("A"), CircuitNode("AAA"), CircuitNode("B")),
			components = Seq(
				CircuitResistor(nodes = Seq(CircuitNode("A"), CircuitNode("AAA")), value = 6),
				CircuitResistor(nodes = Seq(CircuitNode("AAA"), CircuitNode("B")), value = 6)))
		val cc = c.cleanCircuit(Seq(CircuitNode("A"), CircuitNode("B")))
		assert(cc.components == Seq(
			CircuitResistor(nodes = Seq(CircuitNode("A"), CircuitNode("1")), value = 6, resistorNumber = 3),
			CircuitResistor(nodes = Seq(CircuitNode("1"), CircuitNode("B")), value = 6, resistorNumber = 4)))
	}

	test("testCleanCircuitTwoResistorsSpice") {
		val c = Circuit(
			nodes = Seq(CircuitNode("A"), CircuitNode("AAA"), CircuitNode("B")),
			components = Seq(
				CircuitResistor(nodes = Seq(CircuitNode("A"), CircuitNode("AAA")), value = 6),
				CircuitResistor(nodes = Seq(CircuitNode("AAA"), CircuitNode("B")), value = 6)))
		val cc = c.cleanCircuit(Seq(CircuitNode("A"), CircuitNode("B"))).toUndecoratedSpice
		assert(cc == "R3 A 1 10u\nR4 1 B 10u")
	}
}
