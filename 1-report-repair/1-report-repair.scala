import scala.io.Source
object ReportRepair {
	def main(args: Array[String]): Unit = {
		val report = Source.fromFile(args(1)).getLines()
		val sorted = report.map(_.toInt).toIndexedSeq.sorted
		val answer = args(0) match {
			case "from2" => from2(sorted)
			case "from3" => from3(sorted)
		}
		println(answer)
	}

	def from2(report: IndexedSeq[Int], target: Int = 2020): Option[Int] = {
		var i = 0
		var j = report.size - 1
		while (i < j) {
			report(i) + report(j) match {
				case `target` => return Some(report(i) * report(j))
				case sum if sum < target => i += 1
				case sum if sum > target => j -= 1
			}
		}
		return None
	}

	def from3(report: IndexedSeq[Int]): Option[Int] = {
		var i = 0
		while (i < report.size) from2(report.patch(i, Seq.empty, 1), 2020 - report(i)) match {
			case Some(ans) => return Some(ans * report(i))
			case None => i += 1
		}
		None
	}
}