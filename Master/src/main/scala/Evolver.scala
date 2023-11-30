
class Evolver(
	val population: Population
) {
	def run(): Unit = {
		var pop = population
		var bestFitness = 1E100
		var lastBestFitness = 1E100
		//for (_ <- 1 to Parameters.numGenerations) {
		do {
			println(s"Measuring population #${Population.generation} fitness")
			val measuredPopulation = pop.measurePopulationFitness()
			bestFitness = measuredPopulation.head.fitness
			if (lastBestFitness != bestFitness) {
				pop.writeToFile(fileName = Parameters.workDirectory.resolve(s"population_gen${Population.generation}.json").toString)
				CircuitPlotter.plotCircuit(measuredPopulation.head, Population.generation)
				println("Best individual:")
				println(s"\nGenerator:${measuredPopulation.head.generator.toString}")
				println(s"\nCircuit:${measuredPopulation.head.circuit.toUndecoratedSpice}")
				lastBestFitness = bestFitness
			}
			val gb = 1024 * 1024 * 1024.0
			val runtime = Runtime.getRuntime
			println(f"** Used Memory: ${(runtime.totalMemory - runtime.freeMemory) / gb}%6.2f GB")
			println(f"** Free Memory: ${runtime.freeMemory / gb}%6.2f GB")
			println(f"** Total Memory: ${runtime.totalMemory / gb}%5.2f GB")
			println(f"** Max Memory: ${runtime.maxMemory / gb}%7.2f GB")
			println("Generating new population")
			pop = Population.nextPopulation(measuredPopulation)
		} while (bestFitness > Parameters.targetFitness)
	}
}

object Evolver {
	def apply(population: Population): Evolver = new Evolver(population = population)
}
