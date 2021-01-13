package tests

import expressions.Expressions
import org.scalatest._

import scala.collection.immutable.ListMap

class TestBoolean extends FunSuite {

  test("Test Boolean Expressions") {

    val and = (a: Boolean, b: Boolean) => a && b
    val or = (a: Boolean, b: Boolean) => a || b
    val xor = (a: Boolean, b: Boolean) => (a || b) && !(a && b)
    val implies = (a: Boolean, b: Boolean) => !(a && !b)
    val iff = (a: Boolean, b: Boolean) => (a && b) || (!a && !b)

    val operatorTable: Map[String, (Boolean, Boolean) => Boolean] = Map(
      "&&" -> and,
      "||" -> or,
      "xor" -> xor,
      "->" -> implies,
      "<>" -> iff
    )

    val order = List(List("&&"), List("||", "xor"), List("->", "<>"))


    val expressions: Map[String, Boolean] = ListMap(
      "true && false" -> false,
      "true&&false" -> false,
      "true || false" -> true,
      "true <> false" -> false,
      "(true -> false) <> (false || false)" -> true,
      "(true -> false) <> (false || true)" -> false,
      "(true -> false) <> (false && true)" -> true,
      "(false -> true) <> (false && true)" -> false,
      "(false -> true) <> (true && true)" -> true,
      "false || true && false" -> false,
      "true || false && false -> false xor true && false" -> false,
      "true||false&&false->falsexortrue&&false" -> false
    )

    for ((expression, expected) <- expressions) {
      val computed = Expressions.
        evaluate(expression, (s: String) => s.toBoolean, operatorTable, order)
      assert(computed == expected, expression + " <---> " + computed)
    }

  }

}
