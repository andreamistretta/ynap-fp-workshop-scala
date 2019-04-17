package textgame

import scala.io.StdIn._

class Game {

  import Domain._
  import Logic._

  object Domain {

    case class Position(x: Int, y: Int) {
      def update(newX: Int, newY: Int): Position =
        Position(x + newX, y + newY)
    }

    sealed trait Error
    case object UnknownCommand  extends Error
    case object UnknownMovement extends Error
    case object NoCommand    extends Error
    case object WrongMovement   extends Error

    sealed trait Command
    case object Quit             extends Command
    case object Show             extends Command
    case object Help             extends Command
    case class Move(m: Movement) extends Command
    case class Bad(e: Error)     extends Command

    sealed trait Movement
    case object MoveU extends Movement
    case object MoveR extends Movement
    case object MoveD extends Movement
    case object MoveL extends Movement

    case class Continue(value: Boolean)

    case class Player(name: String, pos: Position)

    case class Delta(x: Int, y: Int)

    case class GameWorld(player: Player, field: Field[String])

    case class GameExecution(world: GameWorld, continue: Continue)

    case class Field[A](grid: Vector[Vector[A]]) {
      def check(pos: Position): Position = {
        val size = grid.size - 1
        if (pos.x < 0
            || pos.y < 0
            || pos.x > size
            || pos.y > size) throw new Exception("Invalid direction")
        else pos
      }
    }

    object Player {
      def begin() = {
        println("What is your name?")
        val name = readLine().trim
        println(s"Hello, $name, welcome to the game!")
        Player(name, Position(0, 0))
      }
    }

    object Field {
      def mk20x20 = Field(Vector.fill(20, 20)("-"))
    }
  }

  object Logic {

    val enter = System.getProperty("line.separator")

    def initWorld(): GameWorld = {
      val world = GameWorld(Player.begin(), Field.mk20x20)
      println("Use commands to play")
      world
    }

    def gameLoop(world: GameWorld): Unit = {
      val execution = execCommand(gameStep(), world)
      if (execution.continue.value)
        gameLoop(execution.world)
    }

    def gameStep(): Command = {
      val line = readLine()
      if (line.length > 0) {
        val words = line.trim.toLowerCase.split("\\s+").toList
        words(0) match {
          case "help" => Help
          case "show" => Show
          case "move" => parseMovement(words)
          case "quit" => Quit
          case _      => Bad(UnknownCommand)
        }
      } else Bad(NoCommand)
    }

    def parseMovement(words: List[String]) =
      if (words.length < 2) Bad(WrongMovement)
      else
        words(1) match {
          case "up"    => Move(MoveU)
          case "down"  => Move(MoveD)
          case "right" => Move(MoveR)
          case "left"  => Move(MoveL)
          case _       => Bad(UnknownMovement)
        }

    def execCommand(command: Command, world: GameWorld): GameExecution =
      command match {
        case Show              => printWorld(world); GameExecution(world, Continue(true))
        case Help              => printHelp(); GameExecution(world, Continue(true))
        case Quit              => printQuit(world.player); GameExecution(world, Continue(false))
        case Move(m: Movement) => GameExecution(handleMovement(m, world), Continue(true))
        case Bad(e: Error)     => handleError(e); GameExecution(world, Continue(true))
      }

    def handleMovement(movement: Movement, world: GameWorld): GameWorld =
      try {
        movement match {
          case MoveU => move(world, Delta(-1, 0))
          case MoveD => move(world, Delta(1, 0))
          case MoveR => move(world, Delta(0, 1))
          case MoveL => move(world, Delta(0, -1))
        }
      } catch {
        case e: Exception => println(e.getMessage); world
      }
    def handleError(error: Error) =
      error match {
        case UnknownCommand  => println("Unknown command")
        case WrongMovement   => println("Missing direction")
        case NoCommand    => ()
        case UnknownMovement => println("Unknown direction")
      }

    def move(world: GameWorld, delta: Delta): GameWorld = {
      val newPos: Domain.Position = world.player.pos.update(delta.x, delta.y)
      world.copy(world.player.copy(pos = world.field.check(newPos)))
    }

    def printWorld(world: GameWorld): Unit =
      println(renderWorld(world))

    def printQuit(player: Player): Unit =
      println(s"Bye bye ${player.name}!")

    def printHelp(): Unit = {
      val value =
        s"""|
            |Valid commands:
            |
            | help
            | show
            | move <up|down|left|right>
            | quit
            |""".stripMargin
      println(value)
    }

    def renderWorld(world: GameWorld): String = {
      val x       = world.player.pos.x
      val y       = world.player.pos.y
      val grid    = world.field.grid
      val updated = grid.updated(x, grid(x).updated(y, "x"))

      enter + updated.map(_.mkString(" ")).mkString(enter) + enter
    }
  }

  def run(): Unit =
    gameLoop(initWorld())
}
