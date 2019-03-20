package textgame

import scala.io.StdIn._

class Game {
  import Domain._
  import Logic._

  object Domain {

    case class Position(x: Int, y: Int)

    case class Continue(value: Boolean)

    case class Player(name: String, pos: Position)

    object Player {
      def begin(name: String) = Player(name, Position(0, 0))
    }

    case class Field(grid: Vector[Vector[String]])

    object Field {
      def mk20x20 =
        Field(Vector.fill(20, 20)("-"))
    }

    case class GameWorld(player: Player, field: Field)

    case class GameExecution(world: GameWorld, continue: Continue)
  }

  object Logic {

    val enter = System.getProperty("line.separator")

    def initWorld(): GameWorld = {
      val world = GameWorld(Player.begin(askName()), Field.mk20x20)
      println("Use commands to play")
      world
    }

    def askName(): String = {
      println("What is your name?")
      val name = readLine().trim
      println(s"Hello, $name, welcome to the game!")
      name
    }

    def gameLoop(world: GameWorld): Unit = {
      val execution = gameStep(world)
      if (execution.continue.value)
        gameLoop(execution.world)
    }

    def gameStep(world: GameWorld): GameExecution = {
      val line = readLine()

      if (line.length > 0) {
        val words = line.trim.toLowerCase.split("\\s+")
        words(0) match {

          case "help" => {
            printHelp()
            GameExecution(world, Continue(true))
          }

          case "show" => {
            printWorld(world)
            GameExecution(world, Continue(true))
          }

          case "move" => {
            if (words.length < 2) {
              println("Missing direction")
              GameExecution(world, Continue(true))
            }
            else {
              try {
                words(1) match {
                  case "up"    => GameExecution(move(world, Position(-1, 0)), Continue(true))
                  case "down"  => GameExecution(move(world, Position(1, 0)), Continue(true))
                  case "right" => GameExecution(move(world, Position(0, 1)), Continue(true))
                  case "left"  => GameExecution(move(world, Position(0, -1)), Continue(true))
                  case _       =>
                    println("Unknown direction")
                    GameExecution(world, Continue(true))
                }
              } catch {
                case e: Exception =>
                  println(e.getMessage)
                  GameExecution(world, Continue(true))
              }
            }
          }

          case "quit" => {
            printQuit(world.player)
            GameExecution(world, Continue(false))
          }

          case _ =>
            println("Unknown command")
            GameExecution(world, Continue(true))
        }
      } else GameExecution(world, Continue(true))
    }

    def move(world: GameWorld, delta: Position): GameWorld = {
      val newX = world.player.pos.x + delta.x
      val newY = world.player.pos.y + delta.y

      val size = world.field.grid.size - 1
      if (newX < 0
          || newY < 0
          || newX > size
          || newY > size) throw new Exception("Invalid direction")

      world.copy(world.player.copy(pos = Position(newX, newY)))
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

  def run(): Unit = {
    gameLoop(initWorld())
  }
}
