import java.io.PrintWriter
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
		val filename = s"plot_test_$generation.cir"
		writeToFileForPlotting(c, filename)
		import scala.sys.process._
		val simCommand = s"ngspice  -b $filename"
		plotter = simCommand.run(log = ProcessLogger(_ => {}))
		//		new File(filename).delete()
	}

}
