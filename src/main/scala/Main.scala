import java.nio.file.{Files,Paths}

object Main {
  def main(args: Array[String]): Unit = {
    println("Gaia: Electronic Circuit Evolution")
    println("==================================")
    val pop = if (args.length > 0 && Files.exists(Paths.get(args(0)))) {
      println(s"Reading population from file [${args(0)}]")
      readPopulationFromFile(args(0))
    } else {
      println(s"Creating new population...")
      Population.initialPopulation()
    }
    Evolver(pop).run()
  }

  private def readPopulationFromFile(fileName: String): Population = {
    import scala.util.Using
    Using(io.Source.fromFile(fileName)) {
      source => {
        val inputPopString = source.mkString
        println("File read. Converting to JSON")
        val inputJSON = ujson.read(inputPopString).obj
        println("JSON object now available. Deserializing population.")
        Population.fromJson(inputJSON)
      }
    }.get
  }
}
