package exercises

import minitest._

/*
 * When we compose functions in fact we chain them.
 *
 * f: A => B
 * g: B => C
 * f andThen g
 *
 * Effectful functions can't be combine in the normal
 * way because after a function invocation we don't have
 * a result value but a description of that result.
 * For example: maybe there is a value or maybe not.
 *
 * There are many operators that enable effectful function
 * combination but two are the most important: map and flatMap.
 *
 * The map function is birng to us by the Functor concept
 * and the flatMap function from the Monad concept.
 */

object MapTests extends SimpleTestSuite {

  /*
   * TODO: Implements toiTry function.
   *       Use the Try effect instead of throws.
   */

  import scala.util.{Failure, Success, Try}

  case class NotAnIntException(s: String) extends RuntimeException(s"not an int: $s")

  val toi: String => Int =
    s =>
      if (s.matches("^[0-9]+$")) s.toInt
      else throw NotAnIntException(s)

  val toiTry: String => Try[Int] =
    s =>
      if (s.matches("^[0-9]+$")) Success(s.toInt)
      else Failure(NotAnIntException(s))

  val dec: Int => Int =
    n => n - 1

  val tos: Int => String =
    n => n.toString

  test("chain one function") {
    val program: String => Try[Int] =
      s => toiTry(s).map(dec)
//      toi.andThen(dec)

      val result = program("10")
      assertEquals(result, Success(9))
  }

  test("chain two functions") {
    val program: String => Try[String] =
      s => toiTry(s).map(dec.andThen(tos))
//      toi.andThen(dec).andThen(tos)

    val result = program("10")
    assertEquals(result, Success("9"))
  }

  test("fail") {
    val program: String => Try[String] =
      s => toiTry(s).map(dec.andThen(tos))
    val result = program("foo")
    assertEquals(result, Failure(NotAnIntException("foo")))
    //      toi.andThen(dec).andThen(tos)
    //    intercept[NotAnIntException] {
    //      program("foo"); ()
    //    }
  }
}
