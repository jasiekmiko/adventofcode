ffimport scala.io.Source
import scala.collection.mutable
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import scala.collection.mutable.Queue
import scala.collection.mutable.Stack

object HandyHaversacks {
	def main(args: Array[String]): Unit = {
		val lines = Source.fromFile(args(1)).getLines()
		val answer = args(0) match {
			case "PossibleOuterBags" => PossibleOuterBags(lines)
			case "NumberOfContainedBags" => NumberOfContainedBags(lines)
		}
		println(answer)
	}

	val Rule = raw"(?<container>.*) bags contain (?<containees>.*)\.".r
	val Containee = raw" ?(?<quantity>\d+) (?<containee>.*) bags?".r
	
	def PossibleOuterBags(rules: Iterator[String]): Int = {
		val edges = rules.flatMap(parseToEdgesFromContaineeToContainer(_)).toList
			.groupBy(_._1)
			.view.mapValues { edgesFromSameSource => edgesFromSameSource.map(_._2) }
			.toMap.withDefaultValue(List.empty)

		val explored = HashSet("shiny gold")
		val queue: Queue[String] = Queue.from(edges("shiny gold"))
		while (queue.nonEmpty) {
			val bag = queue.dequeue()
			if (!explored.contains(bag)) {
				explored.addOne(bag)
				queue.enqueueAll(edges(bag))
			}
		}
		explored.size - 1
	}

	def parseToEdgesFromContaineeToContainer(rule: String): Seq[(String, String)] = rule match {
		case Rule(container, "no other bags") => Seq.empty
		case Rule(container, containees) =>
			containees.split(',').map { 
				case Containee(_, containee) => containee -> container
			}
		case _ => throw new RuntimeException("Invalid rule " + rule)
	}
	
	def NumberOfContainedBags(rules: Iterator[String]): Int = {
		val edges = rules.flatMap(parseToEdges(_)).toList
			.groupBy(_._1)
			.view.mapValues { edgesFromSameSource => edgesFromSameSource.map(_._2) }
			.toMap.withDefaultValue(List.empty)
		println(edges)

		val countBagsHeldBy: String => Int = memoize { root =>
			println(s"first time seeing $root, let's calcualte")
			edges(root).map { case (quantity, bag) =>
				quantity * (countBagsHeldBy(bag) + 1) 
			}.sum		
		}
		
		countBagsHeldBy("shiny gold")
	}

	def parseToEdges(rule: String): Seq[(String, (Int, String))] = rule match {
		case Rule(container, "no other bags") => Seq.empty
		case Rule(container, containees) =>
			containees.split(',').map { 
				case Containee(quantity, containee) => container -> ((quantity.toInt, containee))
			}
		case _ => throw new RuntimeException("Invalid rule " + rule)
	}

	def memoize[I, O](f: I => O): I => O = { 
		println("Creating mem")
		val mem = mutable.Map.empty[I, O]
  		return key => {
			println(s"checking $key")
			if (mem.contains(key)) println("memoiezed $key")
  			val res = mem.getOrElseUpdate(key, f(key))
  		  	println(s"$key contains $res")
  		  	res
  		}
		
	}
}