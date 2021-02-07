import scala.io.Source
object PasswordPhilosophy {
	def main(args: Array[String]): Unit = {
		val passwords = Source.fromFile(args(1)).getLines()
		val answer = args(0) match {
			case "countValid" => countValid(passwords)
			case "countValidToboggan" => countValidToboggan(passwords)
		}
		println(answer)
	}

	def countValid(passwords: Iterator[String]): Int = {
		passwords.count {
			case PasswordRegex(lowerBound, higherBound, character, password) =>
				val occurences = password.count{ c => c == character.head }
				lowerBound.toInt <= occurences && occurences <= higherBound.toInt
		}

	}

	def countValidToboggan(passwords: Iterator[String]): Int = {
		passwords.count {
			case PasswordRegex(pos1, pos2, validationCharacter, password) =>
				val char1 = password(pos1.toInt - 1)
				val char2 = password(pos2.toInt - 1)
				
				char1 != char2 && (char1.toString == validationCharacter || char2.toString == validationCharacter)
		}

	}

	val PasswordRegex = raw"(?<lowerBound>\d+)-(?<higherBound>\d+) (?<character>.): (?<password>.+)".r
}