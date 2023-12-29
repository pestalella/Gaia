import GaiaCommon.{EvalCommand, FitnessError, FitnessResult, NodeStartAcknowledge, NodeStartRequest}
import akka.actor.{Actor, ActorRef, Status}
import akka.event.Logging
import akka.util.Timeout

import scala.language.postfixOps
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.collection.parallel.CollectionConverters._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

case class PendingResponse(result: FitnessResult, sender: ActorRef)

class FitnessEvaluatorActor extends Actor {
	private val eval = CircuitEvaluator
	private val log = Logging(this)

	def receive:  Receive  = connectToMaster

	def connectToMaster: Receive = {
		case ConnectToMaster =>
			val path = "akka://LocalFitnessSystem@localhost:5555/user/FitnessRequester"
			implicit val resolveTimeout: Timeout = Timeout(5 seconds)
			for (master: ActorRef <- context.actorSelection(path).resolveOne()) {
				println("Found master node")
				context.watch(master)
				master ! NodeStartRequest
			}
		case NodeStartAcknowledge =>
			println("The master node acknowledged our request")
			context.become(connected)
			context.setReceiveTimeout(30 seconds)
		case ReceiveTimeout =>
			println("Couldn't connect to the master node. Retrying")
			self ! ConnectToMaster
	}
	def connected: Receive = {
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
		case ReceiveTimeout =>
			println("Lost connection to Master. Reconnecting.")
			context.become(connectToMaster)
			context.setReceiveTimeout(30 seconds)
			self ! ConnectToMaster
	}

	private def runCommand(command: EvalCommand, sender: ActorRef): Future[PendingResponse] = Future {
		val fitEval = LowPassFilter(limitFreq = 5000)
		val measurements = command.circuits.zipWithIndex.par.map(m =>
			(m._2, eval.calcFitness(m._1, fitEval))).toIndexedSeq
		PendingResponse(
			result = FitnessResult(
				transactionID = command.transactionID,
				fitness = measurements.map(_._2)
			),
			sender = sender
		)
	}
}
