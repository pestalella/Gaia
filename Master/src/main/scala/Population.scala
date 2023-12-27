
import java.io.PrintWriter

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.postfixOps
import scala.util.{Try, Success, Failure}

import ujson._

import GaiaCommon.{FitnessError, FitnessResult}

case class Population(members: Seq[PopulationMember]) {
	def measurePopulationFitness(evaluator: FitnessManager): Seq[PopulationMember] = {
		val circuits = members map (m => m.circuit.toSpice(m.circuitId))
		val measureResult = evaluator.calcFitness(circuits)
		//		println("Population: measurement request sent")
		val measuredFitness = Await.result(measureResult, 20000 seconds)
		val measuredPop = Try {
			members zip measuredFitness map (memberFitness => memberFitness._1.copy(fitness = memberFitness._2 + (memberFitness._1.circuit.components.size * 1000)))
		}
		//		println("Population: measurement received and prepared")
		measuredPop match {
			case Success(measured) => measured.sortWith((a, b) => a.fitness < b.fitness)
			case Failure(s) =>
				println(s"[ERROR] There was a problem measuring population: $s")
				println(s"        Fitness list: $measuredFitness")
				members map (m => m.copy(
					fitness = 1E100
				))
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
		CircuitNPN.reset()
		val externalNodes = Seq(CircuitNode("A"), CircuitNode("B"), CircuitNode("0"))
		val w1 = CircuitWire(nodes = Seq(CircuitNode("A"), CircuitNode("B")))
		val generatedCircuit = CircuitBuilder.applyCommand(w1, generator)
		val cleanCircuit = generatedCircuit.cleanCircuit(externalNodes)
		cleanCircuit
	}

	def printPopulationStatistics(sortedPop: Seq[PopulationMember]): Unit = {
		val totalNodes = sortedPop.foldLeft(0)((accum, m) => accum + m.generator.nodeCount)
		val totalComponents = sortedPop.foldLeft(0)((accum, m) => accum + m.circuit.components.size)
		val totalCircuitNodes = sortedPop.foldLeft(0)((accum, m) => accum + m.circuit.nodes.size)
		val nodeNameSize = sortedPop.foldLeft(0)((accum, m) => accum + m.circuit.nodes.foldLeft(0)((accum, node) => accum + node.name.length))
		println("## Population statistics:")
		println(s"#### Total AST nodes: $totalNodes")
		println(s"#### Total circuit components: $totalComponents")
		println(s"#### Total circuit nodes: $totalCircuitNodes")
		println(s"#### Node name size: ${nodeNameSize / 1024} kB")
	}

	def nextPopulation(sortedPop: Seq[PopulationMember]): Population = {
		val accummulatedFitness = sortedPop.tail.scanLeft(sortedPop.head.copy(fitness = 1.0 / sortedPop.head.fitness))(
			(accumMember, member) => member.copy(fitness = 1.0 / member.fitness + accumMember.fitness))
		val newPop = (for (_ <- 1 until Parameters.populationSize / 2 + 1) yield {
			val parent1 = PopulationUtils.selectMember(accummulatedFitness)
			val parent2 = PopulationUtils.selectMember(accummulatedFitness)
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
		Using(scala.io.Source.fromFile(fileName)) {
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

