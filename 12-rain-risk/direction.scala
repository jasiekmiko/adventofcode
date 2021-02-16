package rainrisk

sealed trait Direction { def degrees: Int }
case object N extends Direction { override val degrees = 0 }
case object W extends Direction { override val degrees = 270 }
case object E extends Direction { override val degrees = 90 }
case object S extends Direction { override val degrees = 180 }

object Direction {
	def rotate(start: Direction, rotation: Int): Direction = (start.degrees + rotation + 360) % 360 match {
		case N.degrees => N
		case W.degrees => W
		case E.degrees => E
		case S.degrees => S
	}
}