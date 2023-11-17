package GaiaCommon

case class FitnessResult(
	transactionID: Int,
	fitness: Seq[Double]
) extends CommandResult
