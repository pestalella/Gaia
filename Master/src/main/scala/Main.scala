import java.nio.file.{Files,Paths}

object Main {
  def main(args: Array[String]): Unit = {
    println("Gaia: Electronic Circuit Evolution")
    println("==================================")
    val pop = if (args.length > 0 && Files.exists(Paths.get(args(0)))) {
      println(s"Reading population from file [${args(0)}]")
      Population.fromFile(args(0))
    } else {
      println(s"Creating new population...")
      Population.initialPopulation()
    }
    Evolver(pop).run()
  }

}
