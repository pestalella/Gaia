
import akka.actor.{ActorRef, ActorSystem, Props}
import akka.util.Timeout
import akka.pattern.ask

import java.util.concurrent.atomic.AtomicInteger

import scala.concurrent.duration._
import scala.concurrent.Future
import scala.language.postfixOps
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success, Try}
import GaiaCommon.EvalCommand
import GaiaCommon.{CommandResult, FitnessError, FitnessResult}

case object Init

class FitnessManager {
	def calcFitness(circuits: Seq[String]): Future[Seq[Double]] = {
		//		println("#########################################################")
		//		println("               Sending compute request")
		//		println("#########################################################")

		val jobPieces = circuits.sliding(1000, 1000)
		//		println(s"Sending ${jobPieces.length} pieces of work")
		implicit val timeout: Timeout = Timeout(30000 seconds)
		val eval = jobPieces.zipWithIndex map (work => {
			val cmd = EvalCommand(transactionID = FitnessManager.transactionID.incrementAndGet(), circuits = work._1, fitnessSelector = "")
			(FitnessManager.evaluator ? AddWork(cmd)).mapTo[CommandResult]
		})
		val futureResults = Future.sequence(eval.toSeq)
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


