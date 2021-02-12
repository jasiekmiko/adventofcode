import scala.io.Source

object BinaryBoarding {
	def main(args: Array[String]): Unit = {
		val lines = Source.fromFile(args(1)).getLines()

		val answer = args(0) match {
			case "highestTicket" => highestTicket(lines)
			case "mySeat" => mySeat(lines)
		}
		println(answer)
	}
 
	def highestTicket(lines: Iterator[String]): Int = {
		lines.map(parseToId).max
	}

	def mySeat(lines: Iterator[String]): Int = {
		val others = lines.map(parseToId).toSeq.sorted
		others.tail.foldLeft(others.head) { case (prev, next) => 
			if (next == prev + 2) return prev + 1
			next
		}
		throw new RuntimeException("Did not find a gap!")
	}

	def parseToId(partitioning :String): Int = {
		var low = 0
		var high = 127
		for (dir <- partitioning.take(7)) dir match {
			case 'B' => low = (low + high) / 2 + 1
			case 'F' => high = (low + high) / 2
		}
		assert(low == high)
		var left = 0
		var right = 7
		for (dir <- partitioning.drop(7)) dir match {
			case 'R' => left = (left + right) / 2 + 1
			case 'L' => right = (left + right) / 2
		}
		assert (right == left)

		low * 8 + left
	}
}