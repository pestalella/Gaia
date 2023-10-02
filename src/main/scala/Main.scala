object Main {
  def main(args: Array[String]): Unit = {
    println("Hello!")
    val commandRoot = ASTRandomizer.randomAST(maxDepth = 6)
    val externalNodes = Seq(CircuitNode("A"), CircuitNode("B"))
    val circuit = generateCircuit(commandRoot, externalNodes)
    val cleanCirc = circuit.cleanCircuit(externalNodes)
    println("AST:")
    println(commandRoot.toJson.toString())
    println("Generated SPICE:")
    println(circuit.toSpice)
    println("Cleaned-up SPICE:")
    println(cleanCirc.toSpice)
    //    val spiceSchematic = generateSpice(circuit)
  }
  def generateCircuit(astRoot: ASTNode, externalNodes: Seq[CircuitNode]) : Circuit = {
    val w1 = CircuitWire(nodes = externalNodes)
    w1.applyCommand(astRoot)
  }
}
