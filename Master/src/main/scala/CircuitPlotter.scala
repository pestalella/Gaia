import java.io.{File, PrintWriter}
import java.nio.file.Paths
import scala.sys.process._

object CircuitPlotter {
	private def writeToFileForPlotting(c: Circuit, fileName: String): Unit = {
		new PrintWriter(fileName) {
			write(c.toSpicePlot)
			close()
		}
	}

	def plotCircuit(c: Circuit, generation: Int): Unit = {
		val circuitFileName = s"plot_test_$generation.cir"
		val filename = Parameters.workDirectory.resolve(circuitFileName)
		println(s"Writing circuit to plot in this file: [${filename.toString}]")
		writeToFileForPlotting(c, filename.toString)
		import scala.sys.process._
		val simCommand = s"ngspice -b $circuitFileName"
		println(s"Running this command to plot the circuit: [$simCommand]")
		scala.sys.process.Process(command = simCommand, cwd = new File(Parameters.workDirectory.toString)).!!
		//new File(filename).delete()
	}

}
