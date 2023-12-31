import java.nio.file.Files

object Main {
  def main(args: Array[String]): Unit = {
    println("Gaia: Electronic Circuit Evolution")
    println("==================================")
    Files.createDirectories(Parameters.workDirectory)
    //    val pop = if (args.length == 0) {
    //      println(s"Creating new population...")
    //      Population.initialPopulation()
    //    } else {
    //      val populationFilePath = Parameters.workDirectory.resolve(args(0))
    //      if (Files.exists(populationFilePath)) {
    //        println(s"Reading population from file [$populationFilePath]")
    //        Population.fromFile(populationFilePath.toString)
    //      } else {
    //        println(s"Couldn't open population file at [$populationFilePath]. Creating new population...")
    //        Population.initialPopulation()
    //      }
    //    }
    val columns = 4
    Evolver(
      demes = for (_ <- 1 to columns*columns) yield Population.initialPopulation(),
      columns = columns
    ).run()
  }
}
