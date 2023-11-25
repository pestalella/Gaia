import java.nio.file.{Path, Paths}

object Parameters {
	def mutationRate: Float = 0.3f
	def maxChildHeight : Int = 10

	def populationSize: Int = 50000
	def numGenerations: Int = 32
	def targetFitness: Double = 1000
	def simulationDataPoints: Int = 100

	def workDirectory: Path = Paths.get(System.getProperty("user.home"), "GaiaWork")
}
