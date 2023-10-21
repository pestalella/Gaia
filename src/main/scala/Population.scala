import upickle.default._

import java.io.PrintWriter

case class Population(members: Seq[PopulationMember]) {
	def measurePopulationFitness(): Seq[PopulationMember] = {
		members map (c =>
			PopulationMember(
				circuit = c.circuit,
				generator = c.generator,
				fitness = c.circuit.calcFitness()
			)
		) sortWith ((a, b) => a.fitness < b.fitness)
	}
	def nextGeneration(): Population = {
		val measuredPopulation = measurePopulationFitness()
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
			_ <- 0 until 100
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

	private def nextPopulation(sortedPop: Seq[PopulationMember]): Population = {
		println(s"fitness: ${sortedPop.head.fitness}")
		println(s"pop size: ${sortedPop.size}")
		val accummulatedFitness = sortedPop.tail.scanLeft(sortedPop.head.copy(fitness = 1.0 / sortedPop.head.fitness))(
			(accumMember, member) => member.copy(fitness = 1.0 / member.fitness + accumMember.fitness))
		val newPop = (for (_ <- 1 until sortedPop.size / 2+1) yield {
			val parent1 = Population.selectMember(accummulatedFitness)
			val parent2 = Population.selectMember(accummulatedFitness)
			val Seq(candidate1, candidate2) = ASTOperations.crossover(parent1.generator, parent2.generator)
			val child1 = ASTOperations.mutate(candidate1)
			val child2 = ASTOperations.mutate(candidate2)
			val member1 = if (child1.nodeCount > 50) parent1.generator else child1
			val member2 = if (child2.nodeCount > 50) parent2.generator else child2
			Seq(
				PopulationMember(
					circuit = generateCircuit(member1),
					fitness = 0,
					generator = member1
				),
				PopulationMember(
					circuit = generateCircuit(member2),
					fitness = 0,
					generator = member2
				)
			)
		}).flatten.tail :+ sortedPop.head
		Population(newPop)

	}

	implicit val rw: ReadWriter[Population] = macroRW
}

