import scala.io.Source
object PassportProcessing {
	def main(args: Array[String]): Unit = {
		val lines = Source.fromFile(args(1)).getLines()
		val passports: Seq[Passport] = parse(lines)
		val answer = args(0) match {
			case "countValid" => countValid(passports)
			case "countValidStrict" => countValidStrict(passports)
		}
		println(answer)
	}

	def countValid(passports: Seq[Passport]) = {
		println(s"procesing ${passports.size} passports")
		// passports.foreach{
		// 	println(_)
		// }
		passports.count{
			case Passport(
				Some(_), //byr
				Some(_), //iyr
				Some(_), //eyr
				Some(_), //hgt
				Some(_), //hcl
				Some(_), //ecl
				Some(_), //pid
				_, //cid
			) => true
			case _ => false
		}
	}

	def countValidStrict(passports: Seq[Passport]) = {
		println(s"procesing ${passports.size} passports")
		// passports.foreach{
		// 	println(_)
		// }
		passports.count{
			case Passport(
				Some(byr),
				Some(iyr),
				Some(eyr),
				Some(Height(height, heightUnit)),
				Some(HairColour()),
				Some(EyeColour()),
				Some(PassportId()),
				_, //cid
			) => 
				1920 <= byr.toInt && byr.toInt <= 2002 &&
				2010 <= iyr.toInt && iyr.toInt <= 2020 &&
				2020 <= eyr.toInt && eyr.toInt <= 2030 &&
				(if (heightUnit == "cm") 150 <= height.toInt && height.toInt <= 193 else 59 <= height.toInt && height.toInt <= 76) &&
				true
			case _ => false
		}
	}

	def parse(lines: Iterator[String]): Seq[Passport] = {
		(lines ++ Iterator("")).foldLeft((Seq.empty[Passport], Passport())) { 
			case ((seq, passport), "") => (seq :+ passport, Passport())
			case ((seq, passport), credentials) => (seq, passport.add(credentials))
		}._1
	}

	case class Passport(
		byr: Option[String] = None,
		iyr: Option[String] = None,
		eyr: Option[String] = None,
		hgt: Option[String] = None,
		hcl: Option[String] = None,
		ecl: Option[String] = None,
		pid: Option[String] = None,
		cid: Option[String] = None,
	) {
		def add(credentialsLine: String): Passport = credentialsLine.split(" ").foldLeft(this) {
			case (pass, Credential("byr", value)) => pass.copy(byr = Some(value))
			case (pass, Credential("iyr", value)) => pass.copy(iyr = Some(value))
			case (pass, Credential("eyr", value)) => pass.copy(eyr = Some(value))
			case (pass, Credential("hgt", value)) => pass.copy(hgt = Some(value))
			case (pass, Credential("hcl", value)) => pass.copy(hcl = Some(value))
			case (pass, Credential("ecl", value)) => pass.copy(ecl = Some(value))
			case (pass, Credential("pid", value)) => pass.copy(pid = Some(value))
			case (pass, Credential("cid", value)) => pass.copy(cid = Some(value))
		}
	}

	val Credential = raw"(?<key>[\w]+):(?<value>[#\w]+)".r
	val Height = raw"(?<height>[\d]+)(?<unit>(?:cm)|(?:in))".r
	val HairColour = raw"#[a-f0-9]{6}".r
	val EyeColour = raw"(?:amb)|(?:blu)|(?:brn)|(?:gry)|(?:grn)|(?:hzl)|(?:oth)".r
	val PassportId = raw"[0-9]{9}".r
}