
import akka.actor.{ActorSystem, Props}
import akka.util.Timeout
import akka.pattern.ask

import scala.concurrent.duration._
import scala.concurrent.Future
import scala.language.postfixOps
import GaiaCommon.EvalCommand
import GaiaCommon.CommandResult

case object Init

class LocalFitnessMaster {
	def calcFitness(circuits: Seq[String]): Future[CommandResult] = {
		println("#########################################################")
		println("               Sending compute request")
		println("#########################################################")
		implicit val timeout: Timeout = Timeout(30 seconds)
		val cmd = EvalCommand(transactionID = 1, circuits = circuits, fitnessSelector = "")
		(LocalFitnessMaster.evaluator ? AddWork(cmd)).mapTo[CommandResult]
	}
}

object LocalFitnessMaster {
	var connectionEstablished = false
	implicit val system: ActorSystem = ActorSystem("LocalFitnessSystem")

	private val evaluator = system.actorOf(Props[LocalFitnessRequester], name = "LocalFitnessRequester")

	evaluator ! Init
}


