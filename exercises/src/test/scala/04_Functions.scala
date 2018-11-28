package exercises

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

object FunctionsTests extends SimpleTestSuite {

  /*
   * TODO: implements functions maked with `???`
   */

  val asString: Double => String = in => in.toString

  val parseString: String => Int = in => in.toInt

  val reciprocal: Int => Double = in => 1.0 / in


  def andThen2[A,B,C](f:A => B, g: B=>C): A => C =
    in => g(f(in))

  val reciprocalString: String => String = //in => {
//    andThen2(parseString, andThen2(reciprocal,asString))
    parseString andThen reciprocal andThen asString
//    asString(reciprocal(parseString(in)))



  test("from string to string throught reciprocal") {
//    ignore("use existing function to compute a reciprocal in string")
    assertEquals(reciprocalString("42"), "0.023809523809523808")
  }
}
