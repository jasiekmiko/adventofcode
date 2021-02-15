import scala.io.Source

object EncodingError {
	def main(args: Array[String]): Unit = {
		val lines = Source.fromFile(args(1)).getLines().map(BigInt(_)).toSeq

		val answer = args(0) match {
			case "offendingNumber" => offendingNumber(lines, args(2).toInt)
			case "encryptionWeakness" => encryptionWeakness(lines, args(2).toInt)
		}
		println(answer)
	}
 
	def offendingNumber(numbers: Seq[BigInt], preambleSize: Int): BigInt = {
		val memory = numbers.take(preambleSize).toArray
		var i = 0
		for (n <- numbers.drop(preambleSize)) {
			// println(s"examining $n, current memory: ${memory.mkString(",")}")
			if (!isValid(n, memory)) return n
			memory(i) = n
			i = (i + 1) % preambleSize
		}
		throw new RuntimeException("No invalid number found")
	}

	def encryptionWeakness(numbers: Seq[BigInt], preambleSize: Int): BigInt = {
		val target = offendingNumber(numbers, preambleSize)
		var start, end = 0
		var current: BigInt = 0
		while (end <= numbers.size) current.compare(target) match {
			case 1 => 
				current -= numbers(start)
				start += 1
			case -1 =>
				current += numbers(end)
				end += 1
			case 0 => 
				val sequence = numbers.slice(start, end)
				return sequence.min + sequence.max
		}
		throw new RuntimeException("No sequence matching target found")
	}

	def isValid(n: BigInt, memory: Array[BigInt]): Boolean = {
		memory.flatMap(x => memory.map(_+x)).contains(n)
	}
}