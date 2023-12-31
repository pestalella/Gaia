object CircuitBuilder {
	def applyCommand(comp: CircuitComponent, genericCommand: ASTNode): Circuit = {
		genericCommand match {
			case cmd: ASTCapacitor =>
				applyCommand(comp, cmd)
			case cmd: ASTResistor =>
				applyCommand(comp, cmd)
			case cmd: ASTInductor =>
				applyCommand(comp, cmd)
			case cmd: ASTNPN =>
				applyCommand(comp, cmd)
			case cmd: ASTParallel =>
				applyCommand(comp, cmd)
			case cmd: ASTSeries =>
				applyCommand(comp, cmd)
			case cmd: ASTThreeGND =>
				applyCommand(comp, cmd)
			case cmd: ASTVIA0 =>
				applyCommand(comp, cmd)
			case cmd: ASTVIA1 =>
				applyCommand(comp, cmd)
			case _: ASTEnd => applyCommand(comp)
		}
	}

	def applyCommand(comp: CircuitComponent, cmd: ASTCapacitor): Circuit = {
		applyCommand(
			CircuitCapacitor(
				Seq(comp.nodes.head, comp.nodes.last),
				value = cmd.value
			),
			cmd.cCons)
	}

	def applyCommand(comp: CircuitComponent, cmd: ASTResistor): Circuit = {
		applyCommand(
			CircuitResistor(
				Seq(comp.nodes.head, comp.nodes.last),
				value = cmd.value
			),
			cmd.cCons)
	}

	def applyCommand(comp: CircuitComponent, cmd: ASTInductor): Circuit = {
		applyCommand(
			CircuitInductor(
				Seq(comp.nodes.head, comp.nodes.last),
				value = cmd.value
			),
			cmd.cCons)
	}

	def applyCommand(comp: CircuitComponent, cmd: ASTNPN): Circuit = {
		val baseNode = comp.nodes.head.combined(comp.nodes.last)
		val collectorNode = baseNode.combined(comp.nodes.last)
		val emitterNode = baseNode.combined(collectorNode)
		val base = applyCommand(comp.copy(nodes = Seq(comp.nodes.head, baseNode)), cmd.baseCons)
		val collector = applyCommand(CircuitWire(Seq(comp.nodes.last, collectorNode)), cmd.collectorCons)
		val emitter = applyCommand(CircuitWire(Seq(emitterNode, CircuitNode.ground)), cmd.emitterCons)
		val bjt = applyCommand(
			CircuitNPN(
				Seq(collectorNode, baseNode, emitterNode)
			)
		)
		base.combined(collector).combined(emitter).combined(bjt)
	}

	def applyCommand(comp: CircuitComponent, cmd: ASTParallel): Circuit = {
			val lhs = applyCommand(comp, cmd.aCons)
			val rhs = applyCommand(comp.copy(comp.nodes), cmd.bCons)
			lhs.combined(rhs)
	}
	def applyCommand(comp: CircuitComponent, cmd: ASTSeries): Circuit = {
			val midNode = comp.nodes.head.combined(comp.nodes.last)
			val lhs = applyCommand(comp.copy(nodes = Seq(comp.nodes.head, midNode)), cmd.aCons)
			val rhs = applyCommand(comp.copy(Seq(midNode, comp.nodes.last)),cmd.bCons)
			lhs.combined(rhs)
	}

	def applyCommand(comp: CircuitComponent, cmd: ASTThreeGND): Circuit = {
		val midNode = comp.nodes.head.combined(comp.nodes.last)
		val gndNode = CircuitNode.ground
		val lhs = applyCommand(comp.copy(nodes = Seq(comp.nodes.head, midNode)), cmd.aCons)
		val rhs = applyCommand(CircuitWire(Seq(midNode, comp.nodes.last)), cmd.bCons)
		val gnd = applyCommand(CircuitWire(Seq(midNode, gndNode)), cmd.gndCons)
		lhs.combined(rhs).combined(gnd)
	}

	def applyCommand(comp: CircuitComponent, cmd: ASTVIA0): Circuit = {
		val midNode = comp.nodes.head.combined(comp.nodes.last)
		val viaNode = CircuitNode.via0
		val oldComponentNewNodes = Seq(midNode, comp.nodes.last)
		val oldComponentCircuit = Circuit(
			nodes = oldComponentNewNodes,
			components = Seq(comp.copy(nodes = oldComponentNewNodes))
		)
		val lhs = applyCommand(CircuitWire(Seq(comp.nodes.head, midNode)), cmd.aCons)
		val via = applyCommand(CircuitWire(Seq(midNode, viaNode)), cmd.vCons)
		lhs.combined(oldComponentCircuit).combined(via)
	}

	def applyCommand(comp: CircuitComponent, cmd: ASTVIA1): Circuit = {
		val midNode = comp.nodes.head.combined(comp.nodes.last)
		val viaNode = CircuitNode.via1
		val oldComponentNewNodes = Seq(midNode, comp.nodes.last)
		val oldComponentCircuit = Circuit(
			nodes = oldComponentNewNodes,
			components = Seq(comp.copy(nodes = oldComponentNewNodes))
		)
		val lhs = applyCommand(CircuitWire(Seq(comp.nodes.head, midNode)), cmd.aCons)
		val via = applyCommand(CircuitWire(Seq(midNode, viaNode)), cmd.vCons)
		lhs.combined(oldComponentCircuit).combined(via)
	}

	def applyCommand(comp: CircuitComponent): Circuit =
		Circuit(nodes = comp.nodes, components = Seq(comp))
}

