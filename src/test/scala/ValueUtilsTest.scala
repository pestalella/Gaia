import org.scalatest.BeforeAndAfterEach
import org.scalatest.funsuite.AnyFunSuiteLike

class ValueUtilsTest extends AnyFunSuiteLike {

	test("testBasicCapValue") {
		assert(ValueUtils.valueToCapValue(value = 0) == "10pF")
		assert(ValueUtils.valueToCapValue(value = 1) == "100pF")
		assert(ValueUtils.valueToCapValue(value = 2) == "1nF")
		assert(ValueUtils.valueToCapValue(value = 3) == "10nF")
		assert(ValueUtils.valueToCapValue(value = 4) == "100nF")
		assert(ValueUtils.valueToCapValue(value = 5) == "1uF")
		assert(ValueUtils.valueToCapValue(value = 6) == "10uF")
		assert(ValueUtils.valueToCapValue(value = 7) == "100uF")
		assert(ValueUtils.valueToCapValue(value = 8) == "1mF")
	}

	test("testSmallCapValue") {
		// 10, 12, 15, 18, 22, 27, 33, 39, 47, 56, 68, 82
		assert(ValueUtils.valueToCapValue(value = 1/12.0) == "12pF")
		assert(ValueUtils.valueToCapValue(value = 2/12.0) == "15pF")
		assert(ValueUtils.valueToCapValue(value = 3/12.0) == "18pF")
		assert(ValueUtils.valueToCapValue(value = 4/12.0) == "22pF")
		assert(ValueUtils.valueToCapValue(value = 5/12.0) == "26pF")
		assert(ValueUtils.valueToCapValue(value = 6/12.0) == "32pF")
		assert(ValueUtils.valueToCapValue(value = 7/12.0) == "38pF")
		assert(ValueUtils.valueToCapValue(value = 8/12.0) == "46pF")
		assert(ValueUtils.valueToCapValue(value = 9/12.0) == "56pF")
		assert(ValueUtils.valueToCapValue(value = 10/12.0) == "68pF")
		assert(ValueUtils.valueToCapValue(value = 11/12.0) == "83pF")
	}
}