package GaiaCommon

class LowPassFilter(val limitFreq: Double) extends FitnessCalculator {
	override def calc(simulationData: Seq[SimDataPoint]): Double = {
		val dbData = simulationData map (dataPoint => {
			val dbMagnitude = 20 * scala.math.log10(dataPoint.magnitude + 0.0000001)
			SimDataPoint(
				frequency = dataPoint.frequency,
				magnitude = if (dbMagnitude.isNaN) 1E10 else dbMagnitude,
				phase = dataPoint.phase)
		})
		dbData.foldLeft(0.0)((fitAccum, dataPoint) => fitAccum + dataPointFitness(dataPoint))
	}

	private def dataPointFitness(simDataPoint: SimDataPoint): Double = {
		val hiCutOffFreq = limitFreq
		val lowCutOffFreq = 0.0
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

object LowPassFilter {
	def apply(limitFreq: Double): LowPassFilter = new LowPassFilter(limitFreq)
}
