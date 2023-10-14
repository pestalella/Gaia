import upickle.default._

import java.io.{File, PrintWriter}

case class Population(members: Seq[PopulationMember]) {
	def nextGeneration(): Population = {
		val measuredPopulation = members map (c =>
			PopulationMember(
				circuit = c.circuit,
				generator = c.generator,
				fitness = c.circuit.calcFitness()
			))
		val sortedPop = measuredPopulation.sortWith((circA, circB) => circA.fitness < circB.fitness)
		println(s"fitness: ${sortedPop.head.fitness}\ncircuit: ${sortedPop.head.circuit.toSpice}")
		Population(sortedPop)
	}
	def writeToFile(fileName: String): Unit = {
		val contents = write(indent=2, t = this)
		new PrintWriter(fileName) {
			write(contents)
			close()
		}
	}
}

object Population {
	def apply(members: Seq[PopulationMember]): Population = {
		new Population(members)
	}
	def initialPopulation(): Population = {
		Population(for {
			i <- 0 until 10
		} yield {
			val generator = ASTRandomizer.randomAST(maxDepth = 6)
			PopulationMember(
				circuit = generateCircuit(generator),
				fitness = 0,
				generator = generator)
		})
	}

	private def generateCircuit(generator: ASTNode): Circuit = {
		CircuitResistor.reset()
		CircuitCapacitor.reset()
		val externalNodes = Seq(CircuitNode("A"), CircuitNode("B"))
		val w1 = CircuitWire(nodes = externalNodes)
		CircuitBuilder.applyCommand(w1, generator).cleanCircuit(externalNodes)
	}
	implicit val rw: ReadWriter[Population] = macroRW

}

