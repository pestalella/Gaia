
import akka.actor.{ActorRef, ActorSystem, Props}
import akka.util.Timeout
import akka.pattern.ask

import scala.concurrent.duration._
import scala.concurrent.Future
import scala.language.postfixOps
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success, Try}
import GaiaCommon.EvalCommand
import GaiaCommon.{CommandResult, FitnessError, FitnessResult}
import akka.actor.Status.Status

case object Init

class LocalFitnessMaster {
	def calcFitness(circuits: Seq[String]): Future[Seq[Double]] = {
		println("#########################################################")
		println("               Sending compute request")
		println("#########################################################")

		val jobpieces = circuits.sliding(1000, 1000)
		//		println(s"Sending ${jobpieces.length} pieces of work")
		implicit val timeout: Timeout = Timeout(30000 seconds)
		val eval = jobpieces.zipWithIndex map (work => {
			val cmd = EvalCommand(transactionID = work._2, circuits = work._1, fitnessSelector = "")
			(LocalFitnessMaster.evaluator ? AddWork(cmd)).mapTo[CommandResult]
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

object LocalFitnessMaster {
	implicit val system: ActorSystem = ActorSystem("LocalFitnessSystem")
	val evaluator: ActorRef = system.actorOf(Props[LocalFitnessRequester], name = "LocalFitnessRequester")

	evaluator ! Init
}


