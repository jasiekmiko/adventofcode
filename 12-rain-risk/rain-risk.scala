package rainrisk

import scala.io.Source
import rainrisk.Instruction

object RainRisk {
	def main(args: Array[String]): Unit = {
		val lines = Source.fromFile(args(1)).getLines()

		val answer = args(0) match {
			case "normalNavigation" => normalNavigation(lines)
			case "waypointNavigation" => waypointNavigation(lines)
		}
		println(answer)
	}
 
	def normalNavigation(lines: Iterator[String]) = {
		val endPosition = lines.map(Instruction.parse).foldLeft(Position.start()) { 
			case (position, instruction) => position.follow(instruction) 
		}

		Math.abs(endPosition.north) + Math.abs(endPosition.west)
	}

	def waypointNavigation(lines: Iterator[String]) = {
		val endPosition = lines.map(Instruction.parse).foldLeft(PositionWithWaypoint.start()) { 
			case (position, instruction) => position.follow(instruction) 
		}

		Math.abs(endPosition.north) + Math.abs(endPosition.west)
	}

	case class Position(facing: Direction, north: Int, west: Int) {
		def follow(instruction: Instruction): Position = instruction match {
			case movement: Movement => move(movement.direction, movement.distance)
			case Forward(distance) => move(facing, distance)
			case ClockwiseRotation(rotation) => this.copy(facing = Direction.rotate(facing, rotation))
		}

		def move(direction: Direction, distance: Int) = direction match {
			case N => this.copy(north = north + distance)
			case S => this.copy(north = north - distance)
			case E => this.copy(west = west - distance)
			case W => this.copy(west = west + distance)
		}
	}

	object Position {
		def start(): Position = Position(E, 0, 0)
	}

	case class PositionWithWaypoint(north: Int, west: Int, waypointNorth: Int, waypointWest: Int) {
		def follow(instruction: Instruction) = instruction match {
			case movement: Movement => moveWaypoint(movement.direction, movement.distance)
			case ClockwiseRotation(rotation) => rotateWaypoint(rotation)
			case Forward(times) => move(times)
		}

		def moveWaypoint(direction: Direction, distance: Int) = direction match {
			case N => this.copy(waypointNorth = waypointNorth + distance)
			case S => this.copy(waypointNorth = waypointNorth - distance)
			case E => this.copy(waypointWest = waypointWest - distance)
			case W => this.copy(waypointWest = waypointWest + distance)
		}

		def rotateWaypoint(rotation: Int) = (rotation + 360) % 360 match {
			case 0 => this.copy()
			case 90 => this.copy(waypointNorth = waypointWest, waypointWest = -waypointNorth)
			case 180 => this.copy(waypointNorth = -waypointNorth, waypointWest = -waypointWest)
			case 270 => this.copy(waypointNorth = -waypointWest, waypointWest = waypointNorth)
		}

		def move(times: Int) = this.copy(
			north = north + times * waypointNorth, 
			west = west + times * waypointWest
		)
	}

	object PositionWithWaypoint {
		def start(): PositionWithWaypoint = PositionWithWaypoint(0, 0, 1, -10)
	}

}