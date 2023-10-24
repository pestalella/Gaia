
object Main {
  def main(args: Array[String]): Unit = {

    println("Gaia: Electronic Circuit Evolution")
    println("==================================")
    var pop = Population.initialPopulation()
    var bestFitness = 1E100
    //for (_ <- 1 to Parameters.numGenerations) {
    do {
      val measuredPopulation = pop.measurePopulationFitness()
      if (Population.generation % 10 == 1) {
        pop.writeToFile(fileName = "population.json")
        CircuitPlotter.plotCircuit(measuredPopulation.head.circuit, Population.generation)
        println("Best individual:")
        println(s"\nGenerator:${measuredPopulation.head.generator.toString}")
        println(s"\nCircuit:${measuredPopulation.head.circuit.toUndecoratedSpice}")

      }
      bestFitness = measuredPopulation.head.fitness
      pop = Population.nextPopulation(measuredPopulation)
    } while (bestFitness > Parameters.targetFitness)
    printPopulationStatistics(pop)
    pop.writeToFile(fileName = "population.json")
  }

  private def printPopulationStatistics(pop: Population): Unit = {
    val sortedPop = pop.measurePopulationFitness()
    val best = sortedPop.head
    println(s"Best individual:\n$best")
    CircuitPlotter.plotCircuit(best.circuit, Population.generation)
  }
}
