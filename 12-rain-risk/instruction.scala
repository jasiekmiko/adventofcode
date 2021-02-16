package rainrisk

sealed trait Instruction

trait Movement extends Instruction {
	def direction: Direction
	def distance: Int
}

case class West(override val distance: Int) extends Movement {
	override val direction = W
}

case class North(override val distance: Int) extends Movement {
	override val direction = N
}

case class East(override val distance: Int) extends Movement {
	override val direction = E
}

case class South(override val distance: Int) extends Movement {
	override val direction = S
}

case class Forward(val distance: Int) extends Instruction

case class ClockwiseRotation(val rotation: Int) extends Instruction

object Instruction {
	val InstructionString = raw"^(\D)(\d+)$$".r

	def parse(line: String): Instruction = line match {
		case InstructionString("N", int) => North(int.toInt)
		case InstructionString("S", int) => South(int.toInt)
		case InstructionString("E", int) => East(int.toInt)
		case InstructionString("W", int) => West(int.toInt)
		case InstructionString("L", int) => ClockwiseRotation(-int.toInt)
		case InstructionString("R", int) => ClockwiseRotation(int.toInt)
		case InstructionString("F", int) => Forward(int.toInt)
	}
}