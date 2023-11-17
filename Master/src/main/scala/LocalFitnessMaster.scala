
import akka.actor.{ActorSystem, Props}
import akka.util.Timeout
import akka.pattern.ask

import scala.concurrent.duration._
import scala.concurrent.Future
import scala.io.StdIn
import scala.language.postfixOps
import GaiaCommon.EvalCommand
import com.typesafe.config.ConfigFactory
import scala.util.{Failure, Success}
import scala.concurrent.ExecutionContext.Implicits.global
import GaiaCommon.{EvalCommand, FitnessCalculator, CommandResult, FitnessError, FitnessResult}

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
	implicit val system = ActorSystem("LocalFitnessSystem")

	val evaluator = system.actorOf(Props[LocalFitnessRequester], name = "LocalFitnessRequester")

	evaluator ! Init
	//	StdIn.readLine()
	//	system.terminate()
	//	StdIn.readLine()

}


