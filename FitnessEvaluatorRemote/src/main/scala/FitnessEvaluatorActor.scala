import GaiaCommon.{EvalCommand, FitnessCalculator, FitnessError, FitnessResult}
import akka.actor.{Actor, ActorRef, Status}
import akka.event.Logging

import scala.language.postfixOps
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.collection.parallel.CollectionConverters._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success, Try}

case class PendingResponse(result: FitnessResult, sender: ActorRef)

class FitnessEvaluatorActor extends Actor {
	//	private val eval = DummyEval
	private val eval = CircuitEvaluator
	private val log = Logging(this)

	def receive: Receive = {
		case cmd: EvalCommand =>
			log.info(s"Got a command with transaction ID = ${cmd.transactionID} and ${cmd.circuits.size} elements to process from ${sender().path.toString}")
			val evaluation = runCommand(cmd, sender())
			evaluation.onComplete {
				case Success(value) =>
					log.info(s"Got a result. Sending it to the requester [${value.sender.path.toString}]")
					value.sender ! value.result
				case Failure(t) =>
					log.warning("FAILED TO GET A RESULT")
					sender ! FitnessError(transactionID = cmd.transactionID, message = t.getMessage)
			}
	}

	private def runCommand(command: EvalCommand, sender: ActorRef): Future[PendingResponse] = Future {
		val fitEval = GaiaCommon.LowPassFilter(limitFreq = 5000)
		log.info("Starting measurement")
		val measurements = command.circuits.zipWithIndex.par.map(m =>
			(m._2, eval.calcFitness(m._1, fitEval))).toIndexedSeq
		log.info("Measurements done")
		//		assert(measurements.foldLeft(true, -1)(
		//			(last, cur) => (last._1 && (last._2 + 1 == cur._1), cur._1)
		//		)._1)
		//		log.info("Results are ordered")
		PendingResponse(
			result = FitnessResult(
				transactionID = command.transactionID,
				fitness = measurements.map(_._2)
			),
			sender = sender
		)
	}
}
