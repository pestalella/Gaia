package GaiaCommon

case class EvalCommand(
	transactionID: Int,
	circuits: Seq[String],
	fitnessSelector: String
)
