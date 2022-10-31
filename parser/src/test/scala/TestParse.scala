package dev.jtrim777.hcl4s.parser

import dev.jtrim777.hcl4s.lang.dsl._
import dev.jtrim777.hcl4s.lang.expr.{Expression, operators}
import fastparse.JavaWhitespace._
import fastparse._

object TestParse extends App {
  def andEnd[_: P, T](rule: P[_] => P[T]): P[T] = P {
    rule(implicitly[P[_]]) ~ End
  }

  def execute(raw: String): Parsed[Expression] = parse(raw, {p:P[_] => andEnd(Parser.Expression(_))(p) })

  def test(input: String, expected: Expression): Unit = {
    println("--------------")
    println("Input: "+input)
    execute(input) match {
      case Parsed.Success(result, _) =>
        print("Result: ")
        pprint.pprintln(result)

        if (result != expected) {
          print("\u001b[1;31mERROR: \u001b[0m Parse of input \"" + input + "\" did not yield expected result ")
          pprint.pprintln(expected)
          println("\u001b[1;34mParsing trace: \u001b[0m")
//          println(Parser.log.mkString("\n"))
        }
      case failure: Parsed.Failure =>
        println("\u001b[1;31mERROR: \u001b[0m Failed to parse input \"" + input + "\"")
        println(failure.trace().toString)
        println("\u001b[1;34mParsing trace: \u001b[0m")
//        println(Parser.log.mkString("\n"))
    }

//    Parser.log.clear()

    println("")
  }

  def testExpressions(): Unit = {
    test("0", 0)
    test("0x0", 0)
    test("0b0", 0)

    test("42", 42)
    test("0x2a", 42)
    test("0b101010", 42)

    test("0.0", 0f)
    test("0.10", 0.1)
    test("10.0", 10f)
    test("3.14", 3.14)

    test("true", true)
    test("false", false)

    test("null", Null)

    test("[]", seq())
    test("[5]", seq(5))
    test("[5, 7, 6, 4]", seq(5, 7, 6, 4))

    test("{}", obj())
    test("{foo: 7}", obj("foo".asKey -> 7))
    test("{\"foo\": 7}", obj(strAsTemplate("foo") -> 7))
    test("{foo: 7, bar: 8 + 3}", obj("foo".asKey -> 7, "bar".asKey -> Expression.BinaryOp(8, operators.Add, 3)))

    test("\"\"", tmpl())
    test("\"foo\"", tmpl("foo".asLit))
    test("\"${ 5 + 7 }\"", tmpl(interpolate(Expression.BinaryOp(5, operators.Add, 7))))
    test("\"$${ 5 + 7 }\"", tmpl("${ 5 + 7 }".asLit))
    test("\"foo${ 5 }\"", tmpl("foo".asLit, interpolate(5)))
  }

  testExpressions()
}
