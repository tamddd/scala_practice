sealed abstract class DayOfWeek
case object Sunday extends DayOfWeek
case object Monday extends DayOfWeek
case object Tuesday extends DayOfWeek
case object Wednesday extends DayOfWeek
case object Thursday extends DayOfWeek
case object Friday extends DayOfWeek
case object Saturday extends DayOfWeek

object Main extends App {
  val x: DayOfWeek = Sunday

  x match {
    case Sunday => println("Sunday")
    case _ => println("OK")
  }
}