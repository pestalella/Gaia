import GaiaCommon.{EvalCommand, EvalResponse, FitnessCalculator}

import akka.actor.Actor
import scala.collection.parallel.CollectionConverters._

object DummyEval {
	def calcFitness(circuitSpice: String, fitEval: FitnessCalculator): Double = {
		Thread.sleep(10)
		0.1
	}
}

class FitnessEvaluatorActor extends Actor {
	//	private val eval = DummyEval
	private val eval = CircuitEvaluator

	def receive: Receive = {
		case cmd: EvalCommand =>
			println(s"Got a command with transaction ID = ${cmd.transactionID} and ${cmd.circuits.size} elements to process")
			sender ! runCommand(cmd)
		case msg: String =>
			println(s"Received this message: [$msg]")
	}

	def runCommand(command: EvalCommand): EvalResponse = {
		val fitEval = GaiaCommon.LowPassFilter(limitFreq = 5000)
		val measurements = command.circuits.zipWithIndex.par.map(m =>
			(m._2, eval.calcFitness(m._1, fitEval))).toIndexedSeq
		println("Measurements done")
		assert(measurements.foldLeft(true, -1)(
			(last, cur) => (last._1 && (last._2 + 1 == cur._1), cur._1)
		)._1)
		println("Results are ordered")
		EvalResponse(
			transactionID = command.transactionID,
			fitness = measurements.map(_._2)
		)
	}
}
