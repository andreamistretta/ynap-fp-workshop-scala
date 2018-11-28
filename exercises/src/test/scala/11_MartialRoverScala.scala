import minitest._

/*
 * ADT models data while Function models behaviour.
 * A function is simply something that accepts an input value
 * and produces an output value.
 * In more accademic terms it connects a Domain to a Codomain.
 * Functions are described/documented by it's type definition.
 *
 *  f:  InType => OutType
 */

object MartialRoverScala extends SimpleTestSuite {

  sealed trait Movement

  sealed trait Direction

  case object N extends Direction

  case object S extends Direction

  case object E extends Direction

  case object W extends Direction

  def r(dir: Direction): Direction =
    dir match {
      case N => E
      case E => S
      case W => N
      case S => W
    }

  def l(dir: Direction): Direction =
    dir match {
      case N => W
      case E => N
      case W => S
      case S => E
    }

  def f(c:Command): Position =
    c.direction match {
      case N => c.position()
      case E => S
      case W => N
      case S => W
    }

  def b(c:Command): Direction =
    c match {
      case N => W
      case E => N
      case W => S
      case S => E
    }

  case class Command(position: Position, direction: Direction)

  case class Position(x: Int, y: Int)

  case class Planet(dimX: Int, dimY: Int)

  case class Rover(position: Position, direction: Direction, command: Seq[Command])

  object Rover {
    def move(movement: Seq[Command]): Unit = {

    }
  }

  def game(planet: Planet, rover: Rover): Unit = {
    ???
  }

  test("") {

  }
}
