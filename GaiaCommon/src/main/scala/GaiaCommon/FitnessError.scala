package GaiaCommon

case class FitnessError(
	transactionID: Int,
	message: String
) extends CommandResult
