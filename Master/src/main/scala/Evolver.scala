import Evolver.evaluator

import java.util.concurrent.TimeUnit
import collection.mutable.Buffer
import collection.parallel.CollectionConverters._
import concurrent.ExecutionContext.Implicits.global
import scala.collection.mutable

class Evolver(
	val populations: Seq[Island],
	val rows: Int,
	val columns: Int
) {
	var generation = 0
	def run(): Unit = {
		var lastBestFitness = 1E100
		var islands = populations
		do {
			val n = System.nanoTime()
			// Evaluates island populations.
			val measuredPopulations = islands.zipWithIndex.map(indexedIsland => {
				val island = indexedIsland._1
				println(s"Measuring island (${island.column}, ${island.row}) fitness")
				island.measurePopulationFitness(evaluator = evaluator)
			})
			println("Measurement completed")

			for (measuredIsland <- islands) {
				if (lastBestFitness > measuredIsland.bestIndividual.fitness) {
					measuredIsland.population.writeToFile(fileName =
						Parameters.workDirectory.resolve(
							s"population_gen${generation}_island_${measuredIsland.column}_${measuredIsland.row}.json"
						).toString
					)
					CircuitPlotter.plotCircuit(measuredIsland.bestIndividual, generation)
					println(s"Best fitness was found in island (${measuredIsland.column}, ${measuredIsland.row}) value: ${measuredIsland.bestIndividual.fitness}")
					lastBestFitness = measuredIsland.bestIndividual.fitness
				}
			}
			println("Gathering island migrants")
			val islandEmigrants = measuredPopulations map (measuredPopulation => {
				val accummulatedFitness = measuredPopulation.tail.scanLeft(measuredPopulation.head.copy(fitness = 1.0 / measuredPopulation.head.fitness))(
					(accumMember, member) => member.copy(fitness = 1.0 / member.fitness + accumMember.fitness))
				val numEmigrants = (measuredPopulation.size * Parameters.migrantFraction).toInt
				for (_ <- 1 to numEmigrants) yield {
					PopulationUtils.selectMember(accummulatedFitness)
				}
			})
			val immigrants = mutable.Buffer[Seq[PopulationMember]]()
			for (_ <- populations) immigrants.append(Seq())
			for ((emigrants, islandIdx) <- islandEmigrants.zipWithIndex) {
				val column = islandIdx % columns
				val row = islandIdx / columns
				val north = ((row + rows - 1) % rows) * columns + column
				val south = ((row + 1) % rows) * columns + column
				val west = row * columns + (column - 1 + columns) % columns
				val east = row * columns + (column + 1) % columns
				val perIsland = emigrants.grouped(emigrants.size / 4).toSeq
				immigrants(north) = immigrants(north) ++ perIsland(0)
				immigrants(south) = immigrants(south) ++ perIsland(1)
				immigrants(east) = immigrants(east) ++ perIsland(2)
				immigrants(west) = immigrants(west) ++ perIsland(3)
			}
			println("Migration decided")
			val n1 = System.nanoTime()
			println(s"Elapsed time measuring populations: ${TimeUnit.MILLISECONDS.convert(n1 - n, TimeUnit.NANOSECONDS)}ms")
			println("Generating new population")
			islands = for ((island, immigrantsToIsland) <- islands zip immigrants) yield {
				val islandIdx = island.row * columns + island.column
				println(s"Adding ${immigrantsToIsland.size} immigrants to island (${island.column}, ${island.row})")
				val nextPopulation = measuredPopulations(islandIdx).dropRight(immigrantsToIsland.size)
				island.advancePopulation((nextPopulation ++ immigrantsToIsland).sortWith((a, b) => a.fitness < b.fitness))
			}
			val n2 = System.nanoTime()
			println(s"Elapsed time generating new populations: ${TimeUnit.MILLISECONDS.convert(n2 - n1, TimeUnit.NANOSECONDS)}ms")

			printMemoryStats()

			generation += 1
		} while (lastBestFitness > Parameters.targetFitness)
	}

	private def printMemoryStats(): Unit = {
		val runtime = Runtime.getRuntime
		val gb = 1024 * 1024 * 1024.0
		println(f"** Used Memory: ${(runtime.totalMemory - runtime.freeMemory) / gb}%6.2f GB")
		println(f"** Free Memory: ${runtime.freeMemory / gb}%6.2f GB")
		println(f"** Total Memory: ${runtime.totalMemory / gb}%5.2f GB")
		println(f"** Max Memory: ${runtime.maxMemory / gb}%7.2f GB")
	}
}

object Evolver {
	val evaluator = new FitnessManager

	def apply(
		demes: Seq[Population],
		columns: Int,
	): Evolver = {
		var row = 0
		var col = 0
		val islands = demes map (p => {
			val curRow = row
			val curCol = col
			col += 1
			if (col >= columns) {
				col = 0
				row += 1
			}
			new Island(population = p, row = curRow, column = curCol)
		})
		new Evolver(populations = islands, rows = scala.math.ceil(demes.size / columns).toInt, columns = columns)
	}
}
