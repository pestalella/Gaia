import GaiaCommon.SimDataPoint

class BandPassFilter(val lowFreq: Double, val highFreq: Double) extends FitnessCalculator {
	override def calc(simulationData: Seq[SimDataPoint]): Double = {
		val dbData = simulationData map (dataPoint => {
			val dbMagnitude = 20 * scala.math.log10(dataPoint.magnitude + 0.0000001)
			SimDataPoint(
				frequency = dataPoint.frequency,
				magnitude = if (dbMagnitude.isNaN || dbMagnitude.isInfinity) 1E10 else dbMagnitude,
				phase = dataPoint.phase)
		})
		dbData.foldLeft(0.0)((fitAccum, dataPoint) => fitAccum + dataPointFitness(dataPoint))
	}

	private def dataPointFitness(simDataPoint: SimDataPoint): Double = {
		val hiCutOffFreq = highFreq
		val lowCutOffFreq = lowFreq
		if (lowCutOffFreq < simDataPoint.frequency && simDataPoint.frequency < hiCutOffFreq) {
			if (scala.math.abs(simDataPoint.magnitude) < 0.0001)
				0
			else if (scala.math.abs(simDataPoint.magnitude) < 0.6)
				scala.math.abs(simDataPoint.magnitude) * 10
			else
				scala.math.abs(simDataPoint.magnitude) * 100
		} else {
			if (simDataPoint.magnitude < -120.0)
				0
			else if (simDataPoint.magnitude < -60.0)
				scala.math.abs(-120 - simDataPoint.magnitude)
			else
				scala.math.abs(-120 - simDataPoint.magnitude) * 10
		}
	}
}

object BandPassFilter {
	def apply(lowFreq: Double, highFreq: Double): BandPassFilter = new BandPassFilter(lowFreq,highFreq)
}
