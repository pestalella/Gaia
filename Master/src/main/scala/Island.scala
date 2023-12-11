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
		println(s"Best individual fitness on island ($column, $row): ${bestIndividual.fitness}")
		measuredPop
	}

	def advancePopulation(measuredPopulation: Seq[PopulationMember]): Island = {
		new Island(row = row, column = column, population = Population.nextPopulation(measuredPopulation))
	}
}
