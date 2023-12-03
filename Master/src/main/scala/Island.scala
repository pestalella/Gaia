import scala.collection.mutable

class Island(
	val row: Int,
	val column: Int,
	val population: Population
) {
	val inbound = new scala.collection.mutable.Queue[PopulationMember]()
	val outbound = new scala.collection.mutable.Queue[PopulationMember]()

	def measurePopulationFitness(evaluator: FitnessManager): Seq[PopulationMember] = {
		population.measurePopulationFitness(evaluator)
	}
}
