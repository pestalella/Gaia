import org.scalatest.funsuite.AnyFunSuiteLike

class LowPassFilterTest extends AnyFunSuiteLike {

	test("testCalc1point") {
		val eval = LowPassFilter(1000)
			assert(eval.calc(Seq(SimDataPoint(frequency = 10, magnitude = 1, phase = 9))) == 0.0)
	}

	test("testCalc2point") {
		val eval = LowPassFilter(1000)
		assert(eval.calc(Seq(
			SimDataPoint(frequency = 10, magnitude = 1, phase = 0),
			SimDataPoint(frequency = 2000, magnitude = 0, phase = 0))) == 0.0)
	}
	test("testCalcManyPoints") {
		val eval = LowPassFilter(1000)
		val freqs = Seq(1,2,5,10,20,50,100,200,500,1000,2000,5000,10000,20000)
		val simData = freqs map (f => SimDataPoint(frequency = f, magnitude = if (f < 1000) 1 else 0, phase = 0))
		assert(eval.calc(simData) == 0.0)
	}
}
