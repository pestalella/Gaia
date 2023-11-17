import akka.actor.{ActorSystem, Props}
import com.typesafe.config.ConfigFactory
import scala.io.StdIn

object RemoteFitnessEvaluator extends App {
	val config = ConfigFactory.load()
	val system = ActorSystem.create("RemoteFitnessSystem", config);
	system.actorOf(Props[FitnessEvaluatorActor], name = "FitnessEvaluatorActor")

	StdIn.readLine()
	system.terminate()
	StdIn.readLine()
}