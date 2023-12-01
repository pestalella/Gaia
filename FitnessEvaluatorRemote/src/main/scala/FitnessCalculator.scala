import GaiaCommon.SimDataPoint

trait FitnessCalculator {
	def calc(simulationData: Seq[SimDataPoint]): Double
}
