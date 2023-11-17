import GaiaCommon.{EvalCommand, FitnessCalculator, FitnessError, FitnessResult}
import akka.actor.Actor
import akka.event.Logging

import scala.collection.parallel.CollectionConverters._
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.postfixOps
import scala.util.{Failure, Success}

class FitnessEvaluatorActor extends Actor {
	//	private val eval = DummyEval
	private val eval = CircuitEvaluator
	val log = Logging(this)

	def receive: Receive = {
		case cmd: EvalCommand =>
			log.info(s"Got a command with transaction ID = ${cmd.transactionID} and ${cmd.circuits.size} elements to process")
			val evaluation = runCommand(cmd)
			val response = Await.result(evaluation, 200 seconds)
			sender ! response
	}

	def runCommand(command: EvalCommand): Future[FitnessResult] = Future {
		val fitEval = GaiaCommon.LowPassFilter(limitFreq = 5000)
		log.info("Starting measurement")
		val measurements = command.circuits.zipWithIndex.par.map(m =>
			(m._2, eval.calcFitness(m._1, fitEval))).toIndexedSeq
		log.info("Measurements done")
		assert(measurements.foldLeft(true, -1)(
			(last, cur) => (last._1 && (last._2 + 1 == cur._1), cur._1)
		)._1)
		log.info("Results are ordered")
		FitnessResult(
			transactionID = command.transactionID,
			fitness = measurements.map(_._2)
		)
	}
}
