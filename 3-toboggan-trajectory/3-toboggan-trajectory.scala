import scala.io.Source
object TobogganTrajectory {
	def main(args: Array[String]): Unit = {
		val mapLines = Source.fromFile(args(1)).getLines()
		val map = Array.from(mapLines)
		val answer = args(0) match {
			case "3-1-treeCount" => treeCount(map, 3, 1)
			case "checkDifferentSlopes" => checkDifferentSlopes(map)
		}
		println(answer)
	}

	def treeCount(map: Array[String], right: Int, down: Int): Int = {
		val width = map.head.size
		
		var count = 0
		var xPosition = 0
		var yPosition = 0
		do {
			if (isTree(map(yPosition)(xPosition))) {
				count += 1
			}
			xPosition = (xPosition + right) % width
			yPosition = yPosition + down
		} while (yPosition < map.size)
		
		println(s"Slope $right, $down gives $count trees")
		return count
	}

	def checkDifferentSlopes(map: Array[String]): BigInt = {
		BigInt(treeCount(map, 1, 1)) *
		BigInt(treeCount(map, 3, 1)) *
		BigInt(treeCount(map, 5, 1)) *
		BigInt(treeCount(map, 7, 1)) *
		BigInt(treeCount(map, 1, 2))
	}

	private def isTree(char: Char): Boolean = char == '#'
}