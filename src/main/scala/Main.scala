import upickle.default._

object Main {
  def main(args: Array[String]): Unit = {

    println("Gaia: Electronic Circuit Evolution")
    println("==================================")
    var pop = Population.initialPopulation()

    for (_ <- 1 to 100) {
      pop = pop.nextGeneration()
    }
    pop.writeToFile("population.json")

    //val popjson =  pop.members.head
  }
}
