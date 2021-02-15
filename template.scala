import scala.io.Source

object Problem {
	def main(args: Array[String]): Unit = {
		val lines = Source.fromFile(args(1)).getLines()

		val answer = args(0) match {
			case "part1" => part1(lines)
			case "part2" => part2(lines)
		}
		println(answer)
	}
 
	def part1(lines: Iterator[String]) = {
		???
	}

	def part2(lines: Iterator[String]) = {
		???
	}
}