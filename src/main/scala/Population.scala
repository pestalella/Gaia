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
		Population.nextPopulation(measuredPopulation)
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
			i <- 0 until 100
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

	private def selectMember(membersWithAccumFitness: Seq[PopulationMember]): PopulationMember = {
		val totalFitness = membersWithAccumFitness.last.fitness
		val selProb = scala.util.Random.nextFloat()*totalFitness
		var accumWeight = 0.0
		membersWithAccumFitness.find(m => {
			accumWeight = accumWeight + m.fitness
			accumWeight > selProb
		}).get
	}

	private def nextPopulation(measuredPopulation: Seq[PopulationMember]): Population = {
		val sortedPop = measuredPopulation.sortWith((a, b) => a.fitness < b.fitness)
		println(s"fitness: ${sortedPop.head.fitness}\ncircuit: ${sortedPop.head.circuit.toSpice}")
		val accummulatedFitness = sortedPop.tail.scanLeft(sortedPop.head.copy(fitness = 1.0 / sortedPop.head.fitness))(
			(accumMember, member) => member.copy(fitness = 1.0 / member.fitness + accumMember.fitness))
		val newPop = (for (_ <- 1 until sortedPop.size / 2) yield {
			val parent1 = Population.selectMember(accummulatedFitness)
			val parent2 = Population.selectMember(accummulatedFitness)
			val Seq(child1, child2) = ASTCrossover.crossover(parent1.generator, parent2.generator)
			Seq(
				PopulationMember(
					circuit = generateCircuit(child1),
					fitness = 0,
					generator = child1
				),
				PopulationMember(
					circuit = generateCircuit(child2),
					fitness = 0,
					generator = child2
				)
			)
		}).flatten :+ sortedPop.head
		Population(newPop)

	}

	implicit val rw: ReadWriter[Population] = macroRW
}

