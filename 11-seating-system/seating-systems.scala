import scala.io.Source

object SeatingSystems {
	def main(args: Array[String]): Unit = {
		val lines = Source.fromFile(args(1)).getLines()

		val answer = args(0) match {
			case "part1" => part1(lines)
			case "part2" => part2(lines)
		}
		println(answer)
	}
 
	def part1(lines: Iterator[String]) = {
		var prevSeats = parseToSeats(lines)
		var seats = tick(prevSeats)
		while (prevSeats != seats) {
			prevSeats = seats
			seats = tick(seats)
		}
		countOccupied(seats)
	}

	def part2(lines: Iterator[String]) = {
		var prevSeats = parseToSeats(lines)
		var seats = tick2(prevSeats)
		while (prevSeats != seats) {
			prevSeats = seats
			seats = tick2(seats)
		}
		countOccupied(seats)
	}

	def parseToSeats(lines: Iterator[String]): Seatmap = {
		lines.map(_.toSeq).toSeq
	}

	type Seatmap = Seq[Seq[Char]]

	def tick(seatmap: Seatmap): Seatmap = {
		seatmap.zipWithIndex.map { case (row, i) =>
			row.zipWithIndex.map { 
				case ('L', j) if occupiedNeighbours(seatmap, i, j) == 0 => '#'
				case ('#', j) if occupiedNeighbours(seatmap, i, j) >= 4 => 'L'
				case (seat, _) => seat
			}
		} 
	}
	def tick2(seatmap: Seatmap): Seatmap = {
		seatmap.zipWithIndex.map { case (row, i) =>
			row.zipWithIndex.map { 
				case ('L', j) if visibleOccupied(seatmap, i, j) == 0 => '#'
				case ('#', j) if visibleOccupied(seatmap, i, j) >= 5 => 'L'
				case (seat, _) => seat
			}
		} 
	}

	def occupiedNeighbours(seatmap: Seatmap, i: Int, j: Int): Int = {
		val seats = for {
			ii <- Seq(i-1, i, i+1) if 0 <= ii && ii < seatmap.size
			jj <- Seq(j-1, j, j+1) if 0 <= jj && jj < seatmap.head.size && (ii != i || jj != j)
		} yield seatmap(ii)(jj)
		seats.count(_ == '#')
	}

	def visibleOccupied(seatmap: Seatmap, i: Int, j: Int): Int = {
		val directions = Seq((1,0), (1,1), (0,1), (-1, 1), (-1, 0), (-1, -1), (0, -1), (1, -1))
		def occupiedInDirection(direction: (Int, Int)): Int = {
			var k = 1
			def ii = i + k * direction._1
			def jj = j + k * direction._2
			while (0 <= ii && ii < seatmap.size && 0 <= jj && jj < seatmap.head.size) seatmap(ii)(jj) match {
				case '.' => k += 1
				case '#' => return 1
				case 'L' => return 0
			}
			return 0
		
		}
		directions.map(occupiedInDirection(_)).sum
	}

	def countOccupied(seatmap: Seatmap): Int = seatmap.flatten.count(_ == '#')
}