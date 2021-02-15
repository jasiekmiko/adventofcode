import scala.io.Source

object AdapterArray {
	def main(args: Array[String]): Unit = {
		val lines = Source.fromFile(args(1)).getLines().map(_.toInt).toSeq

		val answer = args(0) match {
			case "differences" => differences(lines)
			case "waysToGetToJolt" => waysToGetToJolt(lines)
		}
		println(answer)
	}
 
	def differences(adapters: Seq[Int]): Int = {
		val sorted = adapters.sorted
		val frequencies = (0 +: sorted :+ (sorted.last + 3))
			.sliding(2)
			.map { adapter => adapter(1) - adapter(0) }
			.toSeq
			.groupMapReduce(x => x)(_ => 1)(_ + _)
		frequencies(1) * frequencies(3)
	}

	def waysToGetToJolt(adapters: Seq[Int]): BigInt = {
		val sorted = adapters.sorted
		(sorted :+ (sorted.last + 3))
			.foldLeft (Map(0 -> BigInt(1))) {case (waysToGetThere, adapter) =>
			val next = waysToGetThere.getOrElse(adapter - 3, BigInt(0)) +
				waysToGetThere.getOrElse(adapter - 2, BigInt(0)) +
				waysToGetThere.getOrElse(adapter - 1, BigInt(0))
			waysToGetThere ++ Map((adapter -> next))
		}(sorted.last)
	}
}