package tests

import expressions.Expressions
import org.scalatest._

import scala.collection.immutable.ListMap

class TestArithmetic extends FunSuite {

  val EPSILON: Double = 0.00001

  def equalDoubles(d1: Double, d2: Double): Boolean = {
    (d1 - d2).abs < EPSILON
  }

  test("Test Mathematical Expressions") {

    val pow = (a: Double, b: Double) => Math.pow(a, b)
    val mul = (a: Double, b: Double) => a * b
    val div = (a: Double, b: Double) => a / b
    val add = (a: Double, b: Double) => a + b
    val sub = (a: Double, b: Double) => a - b

    val operatorTable: Map[String, (Double, Double) => Double] = Map(
      "^" -> pow,
      "*" -> mul,
      "/" -> div,
      "+" -> add,
      "-" -> sub
    )

    val orderOfOperations = List(List("^"), List("*", "/"), List("+", "-"))
    
    val expressions: Map[String, Double] = ListMap(
      "2+2" -> 4,
      "1.3+2.5" -> 3.8,
      "2 + 2" -> 4,
      "3+4*3" -> 15,
      "(3+4)*3" -> 21,
      " ( 3      +     4    )   * 3 " -> 21,
      "10 - (8 / 12 * 6 ) / 2 - 1" -> 7,
      "3*3^3" -> 81,
      "15 - (8 + 9 / 3)" -> 4,
      "8.33 + (6.3 - 3.4 / 6.8)" -> 14.13
    )

    for ((expression, expected) <- expressions) {
      val computed = Expressions.evaluate(expression, (s: String) => s.toDouble, operatorTable, orderOfOperations)
      assert(equalDoubles(computed, expected), expression + " <---> " + computed)
    }
  }


  test("Test Mathematical Expressions With Different Operator Names") {

    val pow = (a: Double, b: Double) => Math.pow(a, b)
    val mul = (a: Double, b: Double) => a * b
    val div = (a: Double, b: Double) => a / b
    val add = (a: Double, b: Double) => a + b
    val sub = (a: Double, b: Double) => a - b

    val operatorTable: Map[String, (Double, Double) => Double] = Map(
      "toThePowerOf" -> pow,
      "multipliedBy" -> mul,
      "dividedBy" -> div,
      "plus" -> add,
      "minus" -> sub
    )

    val orderOfOperations = List(List("toThePowerOf"), List("multipliedBy", "dividedBy"), List("plus", "minus"))
    
    val expressions: Map[String, Double] = ListMap(
      "2plus2" -> 4,
      "1.3plus2.5" -> 3.8,
      "2 plus 2" -> 4,
      "3plus4multipliedBy3" -> 15,
      "(3plus4)multipliedBy3" -> 21,
      " ( 3      plus     4    )   multipliedBy 3 " -> 21,
      "10 minus (8 dividedBy 12 multipliedBy 6 ) dividedBy 2 minus 1" -> 7,
      "3multipliedBy3toThePowerOf3" -> 81,
      "15 minus (8 plus 9 dividedBy 3)" -> 4,
      "8.33 plus (6.3 minus 3.4 dividedBy 6.8)" -> 14.13
    )

    for ((expression, expected) <- expressions) {
      val computed = Expressions.evaluate(expression, (s: String) => s.toDouble, operatorTable, orderOfOperations)
      assert(equalDoubles(computed, expected), expression + " <---> " + computed)
    }
  }
}