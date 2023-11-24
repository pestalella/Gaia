import akka.actor.{ActorSystem, Props}
import com.typesafe.config.ConfigFactory
import scala.io.StdIn

object ConnectToMaster

object RemoteFitnessEvaluator extends App {
	val config = ConfigFactory.load()
	val system = ActorSystem.create("RemoteFitnessSystem", config);
	val worker = system.actorOf(Props[FitnessEvaluatorActor], name = "FitnessEvaluatorActor")
	worker ! ConnectToMaster

	StdIn.readLine()
	system.terminate()
	StdIn.readLine()
}