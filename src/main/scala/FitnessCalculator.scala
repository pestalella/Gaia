trait FitnessCalculator {
	def calc(simulationData: Seq[SimDataPoint]): Double
}
