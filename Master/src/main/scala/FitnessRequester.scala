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

class FitnessRequester() extends Actor {
	//	private val path = "akka://RemoteFitnessSystem@10.0.200.227:4444/user/FitnessEvaluatorActor"
	context.setReceiveTimeout(30 seconds)
	private val log = Logging(context.system, this)
	private val pendingWork = new mutable.Queue[WorkRequest]()
	private var pendingResponses = new mutable.Queue[PendingResponse]()
	private val idleNodes = new mutable.Queue[ActorRef]()

	def receive: Receive = identifying

	def identifying: Receive = {
		case AddWork(w) =>
			//			println(s"Got work to do while identifying workers, with trID=${w.transactionID}")
			pendingWork.enqueue(WorkRequest(w, sender))
		case Init =>
			log.info("INIT: waiting for remote actor")
		case NodeStartRequest =>
			// Write down the actor that sent the message
			idleNodes.enqueue(sender())
			println(s"A new remote actor sent a start message [${sender().path.toString}")
			sender ! NodeStartAcknowledge
			context.become(active)
			context.setReceiveTimeout(Duration.Undefined)
			self ! Start

		case ReceiveTimeout => println("timeout")
	}

	private def sendWork(work: EvalCommand): Unit = {
		// Get the first worker
		val worker = idleNodes.dequeue()
		//		println(s"Got work to send. Sending it to ${worker.path.toString}.")
		worker ! work
	}

	def active: Receive = {
		case AddWork(w) =>
			//			println(s"Got work to do, with trID=${w.transactionID}")
			if (idleNodes.nonEmpty) {
				pendingResponses enqueue PendingResponse(transactionID = w.transactionID, requester = sender)
				sendWork(w)
			} else {
				pendingWork.enqueue(WorkRequest(w, sender))
			}
		case NodeStartRequest =>
			// Write down the actor that sent the message
			idleNodes.enqueue(sender())
			println(s"A new remote actor sent a start message [${sender().path.toString}")
			sender ! NodeStartAcknowledge
		case Start =>
			println(s"RECEIVED START from ${sender().path.toString}")
			if (pendingWork.nonEmpty) {
				val workRequest = pendingWork.dequeue()
				pendingResponses enqueue PendingResponse(transactionID = workRequest.work.transactionID, requester = workRequest.requester)
				if (idleNodes.nonEmpty) {
					sendWork(workRequest.work)
				}
			}
		case SendMoreWork =>
			if (pendingWork.nonEmpty) {
				if (idleNodes.nonEmpty) {
					val workRequest = pendingWork.dequeue()
					println(s"Sending work. TransID=${workRequest.work.transactionID}")
					pendingResponses enqueue PendingResponse(transactionID = workRequest.work.transactionID, requester = workRequest.requester)
					sendWork(workRequest.work)
				}
			}
		case r: FitnessResult =>
			val resultSender = sender()
			//			print(s"${r.transactionID} ")
			//			println(s"Got FitnessResult from ${sender.path.toString}")
			val respCandidate = pendingResponses.find(response => response.transactionID == r.transactionID)
			respCandidate match {
				case Some(pendingResponse) =>
					pendingResponse.requester ! r
					pendingResponses = pendingResponses filterNot (p => p.transactionID == r.transactionID)
					if (pendingWork.isEmpty) {
						// Pending work queue is empty. Labeling the node as idle
						idleNodes.enqueue(resultSender)
					} else {
						//						println(s"There's pending work in the queue. Sending it to ${resultSender.path.toString}")
						val workRequest = pendingWork.dequeue()
						pendingResponses enqueue PendingResponse(transactionID = workRequest.work.transactionID, requester = workRequest.requester)
						resultSender ! workRequest.work
					}
					self ! SendMoreWork
				case None => log.warning(s"Received response with an unexpected transactionID: ${r.transactionID}")
			}
		case e: FitnessError => log.info(s"Got FitnessError: $e")

		//		case Terminated(`actor`) =>
		//			println("Receiver terminated")
		//			context.system.terminate()
	}
}
