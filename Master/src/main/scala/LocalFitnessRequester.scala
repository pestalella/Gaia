import GaiaCommon.{EvalCommand, FitnessResult, FitnessError}
import akka.actor._
import akka.util.Timeout
import akka.event.Logging

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.postfixOps
import scala.collection.mutable

case object Start

case class WorkRequest(work: EvalCommand, requester: ActorRef)
case class PendingResponse(transactionID: Int, requester: ActorRef)
case class AddWork(request: EvalCommand)

class LocalFitnessRequester() extends Actor {
	private val path = "akka://RemoteFitnessSystem@127.0.0.1:4444/user/FitnessEvaluatorActor"
	context.setReceiveTimeout(3 seconds)
	private val log = Logging(context.system, this)
	log.info("identifying")
	private var pendingWork = new mutable.Queue[WorkRequest]()
	private var pendingResponses = new mutable.Queue[PendingResponse]()

	def receive: Receive = identifying

	def identifying: Receive = {
		case AddWork(w) =>
			log.info(s"Added work to the pending work queue before INIT")
			pendingWork.enqueue(WorkRequest(w, sender))
		case Init =>
			implicit val resolveTimeout: Timeout = Timeout(5 seconds)
			log.info("INIT: locating remote actor")
			for (ref: ActorRef <- context.actorSelection(path).resolveOne()) {
				log.info("Resolved remote actor ref using Selection")
				context.watch(ref)
				context.become(active(ref))
				context.setReceiveTimeout(Duration.Undefined)
				self ! Start
			}

		case ReceiveTimeout => println("timeout")
	}

	def active(actor: ActorRef): Receive = {
		case AddWork(w) =>
			log.info("Got work to send. Sending now.")
			pendingResponses enqueue PendingResponse(transactionID = w.transactionID, requester = sender)
			actor ! w
		case Start =>
			log.info(s"RECEIVED START from ${sender().path.toString}")
			//actor ! "Hello from the LocalFitnessRequester"
			if (pendingWork.nonEmpty) {
				log.info("Got work to send. Sending now.")
				val workRequest = pendingWork.dequeue()
				pendingResponses enqueue PendingResponse(transactionID = workRequest.work.transactionID, requester = workRequest.requester)
				actor ! workRequest.work
			}
		case r: FitnessResult =>
			log.info(s"Got FitnessResult: $r")
			val respCandidate = pendingResponses.find(response => response.transactionID == r.transactionID)
			respCandidate match {
				case Some(pendingResponse) =>
					pendingResponse.requester ! r
					pendingResponses = pendingResponses filterNot (p => p.transactionID == r.transactionID)
				case None => log.warning(s"Received response with an unexpected transactionID: ${r.transactionID}")
			}
		case e: FitnessError => log.info(s"Got FitnessError: $e")

		case Terminated(`actor`) =>
			println("Receiver terminated")
			context.system.terminate()
	}
}
