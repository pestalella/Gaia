object PopulationUtils {
	def selectMember(membersWithAccumFitness: Seq[PopulationMember]): PopulationMember = {
		val totalFitness = membersWithAccumFitness.last.fitness
		val selProb = scala.util.Random.nextFloat() * totalFitness
		var accumWeight = 0.0
		membersWithAccumFitness.find(m => {
			accumWeight = accumWeight + m.fitness
			accumWeight > selProb
		}).get
	}
}
