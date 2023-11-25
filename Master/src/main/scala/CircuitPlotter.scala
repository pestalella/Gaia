import java.io.{File, PrintWriter}
import java.nio.file.Paths
import scala.sys.process._

object CircuitPlotter {
	private var plotter: Process = "true".run(log = ProcessLogger(_ => {}))
	private def writeToFileForPlotting(c: Circuit, fileName: String): Unit = {
		new PrintWriter(fileName) {
			write(c.toSpicePlot)
			close()
		}
	}

	def plotCircuit(c: Circuit, generation: Int): Unit = {
		if (plotter.isAlive()) plotter.destroy()
		val filename = Parameters.workDirectory.resolve(s"plot_test_$generation.cir")
		writeToFileForPlotting(c, filename.toString)
		import scala.sys.process._
		val simCommand = s"ngspice  -b $filename"
		plotter = simCommand.run(log = ProcessLogger(_ => {}))
		//new File(filename).delete()
	}

}
