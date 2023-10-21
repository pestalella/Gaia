import upickle.default._

object Main {
  def main(args: Array[String]): Unit = {

    println("Gaia: Electronic Circuit Evolution")
    println("==================================")
    var pop = Population.initialPopulation()

    for (_ <- 1 to 100) {
      pop = pop.nextGeneration()
    }
    printPopulationStatistics(pop)
    pop.writeToFile("population.json")
  }

  private def printPopulationStatistics(pop: Population): Unit = {
    val sortedPop = pop.measurePopulationFitness()
    println(s"Best individual:\n${sortedPop.head}")
  }
}
