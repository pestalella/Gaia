import scala.collection.mutable

class Island(
	val row: Int,
	val column: Int,
	val population: Population
) {
	var bestIndividual: PopulationMember = population.members.head
	def measurePopulationFitness(evaluator: FitnessManager): Seq[PopulationMember] = {
		val measuredPop = population.measurePopulationFitness(evaluator)
		bestIndividual = measuredPop.head
		val median: Double = measuredPop(measuredPop.size/2).fitness
		val average: Double = measuredPop.foldLeft(0.0)((total, popMember) => total + popMember.fitness)/measuredPop.size
		println(s"Island ($column, $row): best:${bestIndividual.fitness} median:$median avg:$average")
		measuredPop
	}

	def advancePopulation(measuredPopulation: Seq[PopulationMember]): Island = {
		new Island(row = row, column = column, population = Population.nextPopulation(measuredPopulation))
	}
}
