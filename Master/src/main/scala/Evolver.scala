import Evolver.evaluator

import java.util.concurrent.TimeUnit

import scala.collection.parallel.CollectionConverters._
import scala.concurrent.ExecutionContext.Implicits.global

class Evolver(
	val populations: Seq[Island]
) {
	var generation = 0
	def run(): Unit = {
		var lastBestFitness = 1E100
		var demes = populations
		val demeFitness = scala.collection.mutable.Map[Int, PopulationMember]()
		do {
			val n = System.nanoTime()
			demes = demes.zipWithIndex.par.map(indexedIsland => {
				val island = indexedIsland._1
				//for (_ <- 1 to Parameters.numGenerations) {
				println(s"Measuring deme (${island.column}, ${island.row}) fitness")
				val measuredPopulation = island.measurePopulationFitness(evaluator = evaluator)
				demeFitness(indexedIsland._2) = measuredPopulation.head
				new Island(row = island.row, column = island.column, population = Population.nextPopulation(measuredPopulation))
			}).toIndexedSeq
			val n1 = System.nanoTime()
			println(s"Elapsed time: ${TimeUnit.MILLISECONDS.convert(n1 - n, TimeUnit.NANOSECONDS)}ms")

			for (fitness <- demeFitness) {
				if (lastBestFitness > fitness._2.fitness) {
					val island = demes(fitness._1)
					island.population.writeToFile(fileName =
						Parameters.workDirectory.resolve(
							s"population_gen${generation}_island_${island.column}_${island.row}.json"
						).toString
					)
					CircuitPlotter.plotCircuit(fitness._2, generation)
					println(s"Best fitness was found in island ${fitness._1} value: ${fitness._2}")
					//					println(s"\nGenerator:${fitness._2.generator.toString}")
					//					println(s"\nCircuit:${fitness._2.circuit.toUndecoratedSpice}")
					lastBestFitness = fitness._2.fitness
				}
			}

			val runtime = Runtime.getRuntime
			val gb = 1024 * 1024 * 1024.0
			println(f"** Used Memory: ${(runtime.totalMemory - runtime.freeMemory) / gb}%6.2f GB")
			println(f"** Free Memory: ${runtime.freeMemory / gb}%6.2f GB")
			println(f"** Total Memory: ${runtime.totalMemory / gb}%5.2f GB")
			println(f"** Max Memory: ${runtime.maxMemory / gb}%7.2f GB")
			println("Generating new population")
			generation += 1
		} while (lastBestFitness > Parameters.targetFitness)
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
		new Evolver(islands)
	}
}
