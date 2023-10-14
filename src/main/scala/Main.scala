import upickle.default._

object Main {
  def main(args: Array[String]): Unit = {
    println("Gaia: Electronic Circuit Evolution")
    println("==================================")
    val pop = Population.initialPopulation()
    pop.writeToFile("population0.json")
    val endNode = ASTEnd()
    println(s"endNode = ${write(endNode)}")
    println(s"node = ${write(ASTNumericConstant(12))}")
    println(s"node = ${write(ASTParallel(cConsA = endNode, cConsB = endNode))}")
    println(s"node = ${write(ASTSeries(cConsA = endNode, cConsB = endNode))}")
    println(s"node = ${write(ASTCapacitor(cCons = endNode, valCons=ASTNumericConstant(3)))}")
    println(s"node = ${write(ASTResistor(cCons = endNode, valCons=ASTNumericConstant(3)))}")
    val resistorNode = ASTResistor(cCons = endNode, valCons=ASTNumericConstant(3))
    println(s"node = ${write(ASTThreeGND(aCons = resistorNode, bCons = resistorNode, gndCons = resistorNode))}")

    //val popjson =  pop.members.head
  }
}
