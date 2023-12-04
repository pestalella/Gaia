import GaiaCommon.{EvalCommand, FitnessError, FitnessResult, NodeStartAcknowledge, NodeStartRequest}
import akka.actor.{Actor, ActorRef, Status}
import akka.event.Logging
import akka.util.Timeout

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
		case ConnectToMaster =>
			val path = "akka://LocalFitnessSystem@10.0.200.226:5555/user/FitnessRequester"
			implicit val resolveTimeout: Timeout = Timeout(5 seconds)
			for (master: ActorRef <- context.actorSelection(path).resolveOne()) {
				println("Found master node")
				master ! NodeStartRequest
			}
		case NodeStartAcknowledge =>
			println("The master node acknowledged our request")

		case cmd: EvalCommand =>
			println(s"Got a command with transaction ID = ${cmd.transactionID} and ${cmd.circuits.size} elements to process from ${sender().path.toString}")
			val evaluation = runCommand(cmd, sender())
			evaluation.onComplete {
				case Success(value) =>
					println(s"Got a result. Sending it to the requester [${value.sender.path.toString}]")
					value.sender ! value.result
				case Failure(t) =>
					log.warning("FAILED TO GET A RESULT")
					sender ! FitnessError(transactionID = cmd.transactionID, message = t.getMessage)
			}
	}

	private def runCommand(command: EvalCommand, sender: ActorRef): Future[PendingResponse] = Future {
		val fitEval = LowPassFilter(limitFreq = 5000)
		println("Starting measurement")
		val measurements = command.circuits.zipWithIndex.par.map(m =>
			(m._2, eval.calcFitness(m._1, fitEval))).toIndexedSeq
		println("Measurements done")
		//		assert(measurements.foldLeft(true, -1)(
		//			(last, cur) => (last._1 && (last._2 + 1 == cur._1), cur._1)
		//		)._1)
		//		println("Results are ordered")
		PendingResponse(
			result = FitnessResult(
				transactionID = command.transactionID,
				fitness = measurements.map(_._2)
			),
			sender = sender
		)
	}
}
