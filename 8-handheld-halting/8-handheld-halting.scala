import scala.collection._
import scala.io._

object HandheldHalting {
	def main(args: Array[String]): Unit = {
		val lines = Source.fromFile(args(1)).getLines()
		val answer = args(0) match {
			case "accumulatorBeforeLoop" => accumulatorBeforeLoop(lines)
			case "accumulatorWhenFixed" => accumulatorWhenFixed(lines)
		}
		println(answer)
	}

	def accumulatorBeforeLoop(instructionsIterator: Iterator[String]): Int = {
		accumulatorBeforeLoop(instructionsIterator.toArray, 0, Set.empty[Int])
	}

	def accumulatorBeforeLoop(instructions: Array[String], currentInst: Int, seen: Set[Int]): Int = {
		if (seen.contains(currentInst)) return 0

		val newSeen = seen | Set(currentInst)
		instructions(currentInst) match {
				case Noop(_) 		   => accumulatorBeforeLoop(instructions, currentInst + 1, newSeen)
				case Jump(steps) 	   => accumulatorBeforeLoop(instructions, currentInst + steps.toInt, newSeen)
				case Accumulate(addon) => accumulatorBeforeLoop(instructions, currentInst + 1, newSeen) + addon.toInt
			}
	}

	def accumulatorWhenFixed(instructionsIterator: Iterator[String]): Int = {
		accumulatorWhenFixed(instructionsIterator.toArray :+ "done", 0, Set.empty[Int], false, 0).get
	}

	def accumulatorWhenFixed(instructions: Array[String], currentInst: Int, seen: Set[Int], flipped: Boolean, acc: Int): Option[Int] = {
		if (seen.contains(currentInst)) return None

		val newSeen = seen | Set(currentInst)
		(instructions(currentInst) match {
					case Noop(steps) 	   =>
						accumulatorWhenFixed(instructions, currentInst + 1, newSeen, flipped, acc)
							.orElse(if (flipped) None else accumulatorWhenFixed(instructions, currentInst + steps.toInt, newSeen, true, acc))
					case Jump(steps) 	   => 
						accumulatorWhenFixed(instructions, currentInst + steps.toInt, newSeen, flipped, acc)
							.orElse(if (flipped) None else accumulatorWhenFixed(instructions, currentInst + 1, newSeen, true, acc))
					case Accumulate(addon) => accumulatorWhenFixed(instructions, currentInst + 1, newSeen, flipped, acc + addon.toInt)
					case "done" => Some(acc)
				})
		// .map{ans => println(s"at index $currentInst found ${instructions(currentInst)}, was $flipped fixed and got $acc"); ans}
	}


	val Noop = raw"nop ([+-]\d+)*".r
	val Jump = raw"jmp ([+-]\d+)".r
	val Accumulate = raw"acc ([+-]\d+)".r
}