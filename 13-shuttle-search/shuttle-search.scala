import scala.io.Source

object ShuttleSearch {
	def main(args: Array[String]): Unit = {
		val lines = Source.fromFile(args(1)).getLines()

		val answer = args(0) match {
			case "earliestDeparture" => earliestDeparture(lines)
			case "contest" => contest(lines)
		}
		println(answer)
	}
 
	def earliestDeparture(lines: Iterator[String]) = {
		val startTime = BigInt(lines.next())
		val buslines = lines.next().split(",").filter(_ != "x").map(_.toInt)
		val (firstBus, waitingTime) = buslines
			.map(line => (line, timeToBus(startTime, line)))
			.minBy(_._2)
		firstBus * waitingTime
	}

	def contest(lines: Iterator[String]): BigInt = {
		lines.next()
		val buslines = lines.next().split(",").toIndexedSeq

		var factor = BigInt(1)
		var timestamp = BigInt("100000000000000")
		// var timestamp = BigInt("1")
		buslines.zipWithIndex
			.collect { case (bus, i) if bus != "x" => (bus.toInt, i)}
			.sortBy { case (bus, _) => - bus }
			.foreach {
			    case (bus, i) =>
			    	println (s"finding $bus, $i, currently at timestamp $timestamp and seraching every $factor")
			    	while ((timestamp + i) % bus != 0) timestamp += factor
			    	factor *= bus
			}
		timestamp
	}
	
	def timeToBus(now: BigInt, bus: Int) = bus - now % bus
} 