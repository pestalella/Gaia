
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
		// Wait for evaluator to be ready (to have connected to at least 1 worker)
		implicit val timeout: Timeout = Timeout(30000 seconds)
		var isReady = false
		while (!isReady) {
			val answer = (FitnessManager.evaluator ? AreYouReady).mapTo[Boolean]
			isReady = Await.result(answer, Inf)
		}
		// Connected to a worker. Can begin to distribute work
		val jobPieces = circuits.grouped(2000).toSeq
		val eval = jobPieces.map(work => {
			val cmd = EvalCommand(transactionID = FitnessManager.transactionID.incrementAndGet(), circuits = work, fitnessSelector = "")
			val result = (FitnessManager.evaluator ? AddWork(cmd)).mapTo[CommandResult]
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


