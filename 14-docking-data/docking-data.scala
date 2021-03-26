import scala.io.Source
import scala.collection.mutable.HashMap

object DockingData {
	def main(args: Array[String]): Unit = {
		val lines = Source.fromFile(args(1)).getLines()

		val answer = args(0) match {
			case "maskValues" => maskValues(lines)
			case "maskAddresses" => maskAddresses(lines)
		}
		println(answer)
	}
 
	def maskValues(lines: Iterator[String]): BigInt = {
		var mask = "X"*36
		val map = HashMap.empty[BigInt, BigInt]

		lines.foreach {
			case MaskRegex(newMask) => mask = newMask.reverse
			case UpdateMemory(position, value) => 
				map(BigInt(position)) = applyMaskV1(BigInt(value), mask)
		}

		map.values.sum
	}

	def maskAddresses(lines: Iterator[String]): BigInt = {
		var mask = "X"*36
		val map = HashMap.empty[BigInt, BigInt]

		lines.foreach {
			case MaskRegex(newMask) => mask = newMask.reverse
			case UpdateMemory(position, value) => 
				applyMaskV2(BigInt(position), mask).foreach { map(_) = BigInt(value)}
		}

		map.values.sum
	}

	val MaskRegex = raw"mask = (?<mask>[X10]{36})".r
	val UpdateMemory = raw"mem\[(?<position>\d+)\] = (?<value>\d+)".r

	def applyMaskV1(initValue: BigInt, mask: String): BigInt = {
		mask.zipWithIndex.foldLeft(initValue) {
			case (value, ('X', n)) => value
			case (value, ('1', n)) => value.setBit(n)
			case (value, ('0', n)) => value.clearBit(n)
		}
	}

	def applyMaskV2(initValue: BigInt, mask: String): Seq[BigInt] = {
		mask.zipWithIndex.foldLeft(Seq(initValue)) {
			case (values, ('0', n)) => values
			case (values, ('1', n)) => values.map(_.setBit(n))
			case (values, ('X', n)) => values.flatMap(value => Seq(value.setBit(n), value.clearBit(n)))
		}	
	}
}