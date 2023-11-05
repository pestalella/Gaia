import GaiaCommon.EvalCommand
import akka.actor.{ActorSystem, Props}

import scala.io.StdIn

object RemoteFitnessEvaluator extends App {
	val system = ActorSystem("RemoteDemoSystem")
	val remoteActor = system.actorOf(Props[FitnessEvaluatorActor], name = "FitnessEvaluatorActor")

	val command = EvalCommand(
		transactionID = 1,
		fitnessSelector = "",
		circuits = Seq.iterate("", 100)(_ => "")
	)
	//  remoteActor ! "The FitnessEvaluatorActor is alive"
	remoteActor ! command
	StdIn.readLine()
	system.terminate()
	StdIn.readLine()
}