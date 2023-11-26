
import akka.actor.{ActorSystem, Props}

import java.io.PrintWriter
import java.util.concurrent.TimeUnit
import ujson._

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.postfixOps
import scala.util.{Try, Success, Failure}

import GaiaCommon.{FitnessError, FitnessResult}
import akka.actor.Status

case class Population(members: Seq[PopulationMember]) {
	//	private val evaluator = CircuitEvaluator
	def measurePopulationFitness(): Seq[PopulationMember] = {
		val n = System.nanoTime()
		//		val fitEval = GaiaCommon.LowPassFilter(limitFreq = 5000)
		val measureResult = Population.evaluator.calcFitness(members map (m => m.circuit.toSpice(m.circuitId)))
		println("Population: measurement request sent")
		val measuredFitness = Await.result(measureResult, 20000 seconds)
		println("Population: measurement received")
		val n1 = System.nanoTime()
		println(s"Elapsed time: ${TimeUnit.MILLISECONDS.convert(n1 - n, TimeUnit.NANOSECONDS)}ms")
		val measuredPop = Try {
			members zip measuredFitness map (memberFitness => memberFitness._1.copy(fitness = memberFitness._2))
		}
		measuredPop match {
			case Success(measured) => measured.sortWith((a, b) => a.fitness < b.fitness)
			case Failure(s) =>
				println(s"[ERROR] There was a problem measuring population: $s")
				members
		}
	}

	def toJson: Obj = {
			Obj(
				"members" -> Arr.from(members map (_.toJson))
			)
	}

	def writeToFile(fileName: String): Unit = {
		new PrintWriter(fileName) {
			write(toJson.render(indent=2))
			close()
		}
	}
}

object Population {
	var generation = 0

	val evaluator = new LocalFitnessMaster
	println("Population: evaluator created")

	def apply(members: Seq[PopulationMember]): Population = {
		new Population(members)
	}

	def initialPopulation(): Population = {
		Population(for {
			_ <- 0 until Parameters.populationSize
		} yield {
			val generator = ASTRandomizer.randomAST(maxDepth = 6)
			PopulationMember(
				circuit = generateCircuit(generator),
				fitness = 0,
				generator = generator)
		})
	}

	private def generateCircuit(generator: ASTNode): Circuit = {
		CircuitResistor.reset()
		CircuitCapacitor.reset()
		CircuitInductor.reset()
		val externalNodes = Seq(CircuitNode("A"), CircuitNode("B"))
		val w1 = CircuitWire(nodes = externalNodes)
		CircuitBuilder.applyCommand(w1, generator).cleanCircuit(externalNodes)
	}

	private def selectMember(membersWithAccumFitness: Seq[PopulationMember]): PopulationMember = {
		val totalFitness = membersWithAccumFitness.last.fitness
		val selProb = scala.util.Random.nextFloat()*totalFitness
		var accumWeight = 0.0
		membersWithAccumFitness.find(m => {
			accumWeight = accumWeight + m.fitness
			accumWeight > selProb
		}).get
	}

	def nextPopulation(sortedPop: Seq[PopulationMember]): Population = {
		println(s"Generation $generation fitness: ${sortedPop.head.fitness}")
		val accummulatedFitness = sortedPop.tail.scanLeft(sortedPop.head.copy(fitness = 1.0 / sortedPop.head.fitness))(
			(accumMember, member) => member.copy(fitness = 1.0 / member.fitness + accumMember.fitness))
		val newPop = (for (_ <- 1 until Parameters.populationSize / 2 + 1) yield {
			val parent1 = Population.selectMember(accummulatedFitness)
			val parent2 = Population.selectMember(accummulatedFitness)
			val Seq(candidate1, candidate2) = ASTOperations.crossover(parent1.generator, parent2.generator)
			val child1 = ASTOperations.mutate(candidate1)
			val child2 = ASTOperations.mutate(candidate2)
			val member1 = if (child1.height > Parameters.maxChildHeight) parent1.generator else child1
			val member2 = if (child2.height > Parameters.maxChildHeight) parent2.generator else child2
			Seq(
				PopulationMember(
					circuit = generateCircuit(member1),
					fitness = 0,
					generator = member1
				),
				PopulationMember(
					circuit = generateCircuit(member2),
					fitness = 0,
					generator = member2
				)
			)
		}).flatten.tail :+ sortedPop.head
		generation += 1
		Population(newPop)
	}

	private def fromJson(inputJson: Obj): Population = {
		var currentMember = 0
		val totalMembers = inputJson("members").arr.size
		println(s"Reading a total of $totalMembers members")
		val inputMembers = (for (member <- inputJson("members").arr) yield {
			currentMember += 1
			if (currentMember%100 == 50) println(s"$currentMember/$totalMembers members read.")
			PopulationMember.fromJson(member.obj)
		}).toSeq
		Population(members = inputMembers)
	}

	def fromFile(fileName: String): Population = {
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

