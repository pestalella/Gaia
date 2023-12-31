object ValueUtils {
	private def valueToE12Value(value: Double): String = {
		val siPrefixes = Seq("p", "n", "u", "m", "", "k", "Meg", "G")

		val integerPart = scala.math.floor(value)
		val decimalPart = value - integerPart
		val tenExponent = scala.math.floor(decimalPart*12.0)
		val valTen = scala.math.pow(10, tenExponent)
		val lostExponent = scala.math.pow(10, (integerPart.toInt + 1) % 3)
		val sss = scala.math.round(scala.math.pow(valTen, 1/12.0) * lostExponent)
		val expPlus11 = (value + 1.0).toInt/3
		if (expPlus11 >= 0 && expPlus11 < siPrefixes.size)
			sss.toInt.toString + s"${siPrefixes(expPlus11)}"
		else
			"NaN"
	}
	def valueToCapValue(value: Double): String = {
		valueToE12Value(value) + "F"
	}

	def valueToResistorValue(value: Double): String = {
		valueToE12Value(value)
	}

	def valueToInductorValue(value: Double): String = {
		valueToE12Value(value) + "H"
	}}
