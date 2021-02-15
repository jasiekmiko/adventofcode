import scala.io.Source

object CustomCustoms {
	def main(args: Array[String]): Unit = {
		val lines = Source.fromFile(args(1)).getLines()

		val answer = args(0) match {
			case "countOrYesesPerGroup" => countOrYesesPerGroup(lines)
			case "countAndYesesPerGroup" => countAndYesesPerGroup(lines)
		}
		println(answer)
	}
 
	def countOrYesesPerGroup(lines: Iterator[String]):Int = {
		splitIntoGroups(lines).map { ga =>
			ga.foldLeft(Set.empty[Char]) { case(ors, answers) => ors ++ answers }.size
		}.sum
	}

	def countAndYesesPerGroup(lines: Iterator[String]) = {
		splitIntoGroups(lines).map { ga =>
			val sets = ga.map(_.toSet)
			sets.foldLeft(sets.head) { case(ands, answers) => ands & answers }.size
		}.sum
	}

	def splitIntoGroups(lines: Iterator[String]): Seq[GroupAnswers] = {
		(lines ++ Iterator("")).foldLeft((Seq.empty[GroupAnswers], Seq.empty[Answers])) { 
			case ((gs, g), "") => (gs :+ g, Seq.empty)
			case ((gs, g), line) => (gs, g :+ line)
		}._1
	}

	type GroupAnswers = Seq[Answers]
	type Answers = String
}