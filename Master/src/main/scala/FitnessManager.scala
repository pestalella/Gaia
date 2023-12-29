
import akka.actor.{ActorRef, ActorSystem, Props}
import akka.util.Timeout
import akka.pattern.ask

import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.language.postfixOps
import scala.concurrent.ExecutionContext.Implicits.global
import GaiaCommon.EvalCommand
import GaiaCommon.{CommandResult, FitnessError, FitnessResult}

import scala.concurrent.duration.Duration.Inf

case object Init
case object AreYouReady

class FitnessManager {
	def calcFitness(circuits: Seq[String]): Future[Seq[Double]] = {
		//		println("#########################################################")
		//		println("               Sending compute request")
		//		println("#########################################################")

		// Wait for evaluator to be ready (to have connected to at least 1 worker)
		implicit val timeout: Timeout = Timeout(30000 seconds)
		var isReady = false
		while (!isReady) {
			val answer = (FitnessManager.evaluator ? AreYouReady).mapTo[Boolean]
			isReady = Await.result(answer, Inf)
		}
		val jobPieces = circuits.grouped(2000).toSeq
//		val indexedWork = jobPieces.toSeq
//		val plural = if (indexedWork.size == 1) "" else "s"
//		println(s"Sending ${indexedWork.size} piece$plural of work.")
		val eval = jobPieces.map(work => {
//			println(s"work piece with index ${work._2}")
			val cmd = EvalCommand(transactionID = FitnessManager.transactionID.incrementAndGet(), circuits = work, fitnessSelector = "")
//			println(s"command got a trID=${cmd.transactionID}")
			val result = (FitnessManager.evaluator ? AddWork(cmd)).mapTo[CommandResult]
//			val tt = Await.result(result, Inf)
//			tt match {
//				case e: FitnessError => println(s"#### ERROR #### ${e.message}")
//				case r: FitnessResult => println(s"result: trID=${r.transactionID} fitness=${r.fitness}")
//				case _ => println("WARNING WARNING WARNING!!!")
//			}
			result
		})
		val futureResults = Future.sequence(eval)
		for {res <- futureResults} yield {
			res.foldLeft(Seq[Double]())((accum, status) => status match {
				case r: FitnessResult =>
//					println(s"[MASTER] Got a FitnessResult: trID=${r.transactionID}")
					accum ++ r.fitness
				case e: FitnessError =>
					println(s"Got an error message from the evaluator: [${e.message}")
					accum
			})
		}
	}
}

object FitnessManager {
	var transactionID: AtomicInteger = new AtomicInteger(0)
	implicit val system: ActorSystem = ActorSystem("LocalFitnessSystem")
	val evaluator: ActorRef = system.actorOf(Props[FitnessRequester], name = "FitnessRequester")

	evaluator ! Init
}


