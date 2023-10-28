object Main {
  def main(args: Array[String]): Unit = {
    println("Gaia: Electronic Circuit Evolution")
    println("==================================")
//    var pop = if (args.length > 0) {
//      println(s"Reading population from file ${args(0)}")
//      readPopulationFromFile(args(0))
//    } else {
//      println(s"Creating new population...")
//      Population.initialPopulation()
//    }
    var pop = Population.initialPopulation()
    var bestFitness = 1E100
    var lastBestFitness = 1E100
    //for (_ <- 1 to Parameters.numGenerations) {
    do {
      val measuredPopulation = pop.measurePopulationFitness()
      bestFitness = measuredPopulation.head.fitness
      if ((Population.generation % 10 == 1) &&
          (lastBestFitness != bestFitness)) {
        pop.writeToFile(fileName = "population.json")
        CircuitPlotter.plotCircuit(measuredPopulation.head.circuit, Population.generation)
        println("Best individual:")
        println(s"\nGenerator:${measuredPopulation.head.generator.toString}")
        println(s"\nCircuit:${measuredPopulation.head.circuit.toUndecoratedSpice}")
        lastBestFitness = bestFitness
      }
      pop = Population.nextPopulation(measuredPopulation)
    } while (bestFitness > Parameters.targetFitness)
    printPopulationStatistics(pop)
//    pop.writeToFile(fileName = "population.json")
  }

  private def printPopulationStatistics(pop: Population): Unit = {
    val sortedPop = pop.measurePopulationFitness()
    val best = sortedPop.head
    println(s"Best individual:\n$best")
    CircuitPlotter.plotCircuit(best.circuit, Population.generation)
  }

//  private def readPopulationFromFile(fileName: String): Population = {
//    import scala.util.Using
//    Using(io.Source.fromFile(fileName)) {
//      source => read[Population](source.mkString)
//    }.get
//  }

}
