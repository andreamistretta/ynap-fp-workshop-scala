package textgame

import scala.io.StdIn._

class Game {
  import Domain._
  import Logic._

  object Domain {

    case class Player(name: String, x: Int, y: Int)

    object Player {
      def begin(name: String) = Player(name, 0, 0)
    }

    case class Field(grid: Vector[Vector[String]])

    object Field {
      def mk20x20 =
        Field(Vector.fill(20, 20)("-"))
    }

    case class GameWorld(player: Player, field: Field)

    case class GameExecution(world: GameWorld, exec: Boolean)
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
      if (execution.exec)
        gameLoop(execution.world)
    }

    def gameStep(world: GameWorld): GameExecution = {
      val line = readLine()

      if (line.length > 0) {
        val words = line.trim.toLowerCase.split("\\s+")
        words(0) match {

          case "help" => {
            printHelp()
            GameExecution(world, true)
          }

          case "show" => {
            printWorld(world)
            GameExecution(world, true)
          }

          case "move" => {
            if (words.length < 2) {
              println("Missing direction")
              GameExecution(world, true)
            }
            else {
              try {
                words(1) match {
                  case "up"    => GameExecution(move(world, (-1, 0)), true)
                  case "down"  => GameExecution(move(world, (1, 0)), true)
                  case "right" => GameExecution(move(world, (0, 1)), true)
                  case "left"  => GameExecution(move(world, (0, -1)), true)
                  case _       =>
                    println("Unknown direction")
                    GameExecution(world, true)
                }
              } catch {
                case e: Exception =>
                  println(e.getMessage)
                  GameExecution(world, true)
              }
            }
          }

          case "quit" => {
            printQuit(world.player)
            GameExecution(world, false)
          }

          case _ =>
            println("Unknown command")
            GameExecution(world, true)
        }
      } else GameExecution(world, true)
    }

    def move(world: GameWorld, delta: (Int, Int)): GameWorld = {
      val newX = world.player.x + delta._1
      val newY = world.player.y + delta._2

      val size = world.field.grid.size - 1
      if (newX < 0
          || newY < 0
          || newX > size
          || newY > size) throw new Exception("Invalid direction")

      world.copy(world.player.copy(x = newX, y = newY))
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
      val x       = world.player.x
      val y       = world.player.y
      val grid    = world.field.grid
      val updated = grid.updated(x, grid(x).updated(y, "x"))

      enter + updated.map(_.mkString(" ")).mkString(enter) + enter
    }
  }

  def run(): Unit = {
    gameLoop(initWorld())
  }
}
