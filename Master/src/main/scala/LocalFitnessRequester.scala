import GaiaCommon.{EvalCommand, FitnessError, FitnessResult, NodeStartAcknowledge, NodeStartRequest}
import akka.actor._
import akka.util.Timeout
import akka.event.Logging

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.postfixOps
import scala.collection.mutable

case object Start

case class WorkRequest(work: EvalCommand, requester: ActorRef)
object SendMoreWork
case class PendingResponse(transactionID: Int, requester: ActorRef)
case class AddWork(request: EvalCommand)

class LocalFitnessRequester() extends Actor {
	//	private val path = "akka://RemoteFitnessSystem@10.0.200.227:4444/user/FitnessEvaluatorActor"
	context.setReceiveTimeout(30 seconds)
	private val log = Logging(context.system, this)
	log.info("identifying")
	private var pendingWork = new mutable.Queue[WorkRequest]()
	private var pendingResponses = new mutable.Queue[PendingResponse]()
	private var workingNodes = new mutable.Queue[ActorRef]()

	def receive: Receive = identifying

	def identifying: Receive = {
		case AddWork(w) =>
			log.info(s"Added work to the pending work queue before INIT")
			pendingWork.enqueue(WorkRequest(w, sender))
		case Init =>
			log.info("INIT: locating remote actor")
		//			for (ref: ActorRef <- context.actorSelection(path).resolveOne()) {
		//				log.info("Resolved remote actor ref using Selection")
		//				context.watch(ref)
		//			}
		case NodeStartRequest =>
			// Write down the actor that sent the message
			workingNodes.enqueue(sender())
			println(s"A new remote actor sent a start message [${sender().path.toString}")
			sender ! NodeStartAcknowledge

			context.become(active)
			context.setReceiveTimeout(Duration.Undefined)
			self ! Start

		case ReceiveTimeout => println("timeout")
	}

	private def sendWork(work: EvalCommand) = {
		// Get the first worker
		val worker = workingNodes.dequeue()
		println(s"Got work to send. Sending it to ${worker.path.toString}.")
		worker ! work
		// Return the worker to the end of the queue of workers
		workingNodes.enqueue(worker)
	}

	def active: Receive = {
		case AddWork(w) =>
			pendingResponses enqueue PendingResponse(transactionID = w.transactionID, requester = sender)
			if (workingNodes.nonEmpty) {
				sendWork(w)
			}
		case NodeStartRequest =>
			// Write down the actor that sent the message
			workingNodes.enqueue(sender())
			sender ! NodeStartAcknowledge
		case Start =>
			println(s"RECEIVED START from ${sender().path.toString}")
			//actor ! "Hello from the LocalFitnessRequester"
			if (pendingWork.nonEmpty) {
				val workRequest = pendingWork.dequeue()
				pendingResponses enqueue PendingResponse(transactionID = workRequest.work.transactionID, requester = workRequest.requester)
				if (workingNodes.nonEmpty) {
					sendWork(workRequest.work)
				}
			}
		case SendMoreWork =>
			println(s"RECEIVED SendMoreWork from ${sender().path.toString}")
			if (pendingWork.nonEmpty) {
				val workRequest = pendingWork.dequeue()
				pendingResponses enqueue PendingResponse(transactionID = workRequest.work.transactionID, requester = workRequest.requester)
				if (workingNodes.nonEmpty) {
					sendWork(workRequest.work)
				}
			}
		case r: FitnessResult =>
			println(s"Got FitnessResult from ${sender.path.toString}")
			val respCandidate = pendingResponses.find(response => response.transactionID == r.transactionID)
			respCandidate match {
				case Some(pendingResponse) =>
					println("Sending result to requester")
					pendingResponse.requester ! r
					println("Removing result from pending responses")
					pendingResponses = pendingResponses filterNot (p => p.transactionID == r.transactionID)
					if (pendingWork.nonEmpty)
						self ! SendMoreWork
				case None => log.warning(s"Received response with an unexpected transactionID: ${r.transactionID}")
			}
		case e: FitnessError => log.info(s"Got FitnessError: $e")

		//		case Terminated(`actor`) =>
		//			println("Receiver terminated")
		//			context.system.terminate()
	}
}
