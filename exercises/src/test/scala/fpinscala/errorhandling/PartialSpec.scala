package fpinscala.errorhandling

import org.scalatest._
import Partial._

class PartialSpec extends FlatSpec {
  "Exercise 9 - partial.map(f)" should "pass this tests" in {
    assertResult(Errors(List("E1", "E2"))) { Errors(List("E1", "E2")).map(x => x) }
    assertResult(Success(42)) { Success(21).map(_ * 2) }
    assertResult(Success("42")) { Success(21).map(x => (x * 2).toString) }
  }

  "Exercise 9 - partial.flatMap(f)" should "pass this tests" in {
    def div10(x: Int): Partial[String, Int] =
      try { Success(10 / x) } catch {case e: Exception => Errors(List("Division by zero !"))}

    assertResult(Errors(List("E1", "E2"))) { Errors(List("E1", "E2")) flatMap {x => x} }
    assertResult(Errors(List())) { Errors(List()) flatMap {x => x} }
    assertResult(Success(5)) { Success(2).flatMap(div10) }
    assertResult(Errors(List("Division by zero !"))) { Success(0).flatMap(div10) }
  }

  "Exercise 9 - partial.orElse(o)" should "pass this tests" in {
    assertResult(Success(42))      {Errors(List("Brol")) orElse Success(42) }
    assertResult(Errors(List("Rebrol"))) {Errors(List(78)) orElse Errors(List("Rebrol")) }
    assertResult(Success(42)) { Success(42) orElse Errors(List("Error!")) }
    assertResult(Success(42)) { Success(42) orElse Success(42.42) }
  }

  "Exercise 9 - partial.map2(op)(f)" should "pass this tests" in {
    assertResult(Errors(List("E1","E2","E3"))) { Errors(List("E1")).map2 (Errors(List("E2","E3"))) {(_, _) => 42} }
    assertResult(Errors(List("E1"))) { Errors(List("E1")).map2 (Errors(List())) {(_, _) => 42} }
    assertResult(Errors(List("E2","E3"))) { Errors(List()).map2 (Errors(List("E2","E3"))) {(_, _) => 42} }
    assertResult(Errors(List("E1", "E2"))) { Errors(List("E1", "E2")).map2 (Success("Coucou")) {(_, _) => 42} }
    assertResult(Errors(List("E1"))) { Success("Brol").map2 (Errors(List("E1"))) {(_, _) => 42} }
    assertResult(Success("Brol42")) { Success("Brol").map2 (Success(42)) (_ + _.toString) }
  }

  "Exercise 9 - traverse(list)(f)" should "pass this tests" in {
    def f(x: Int): Partial[String, Int] = if (x > 0) Success(2 * x) else Errors(List(x.toString + " <= 0"))
    assertResult(Success(List())) { traverse(List())(x => x) }

    assertResult(Errors(List("-2 <= 0"))) { traverse(List(21, -2, 39))(f) }
    assertResult(Errors(List("-2 <= 0"))) { traverse(List(21, 39, -2))(f) }
    assertResult(Errors(List("-2 <= 0", "-8 <= 0"))) { traverse(List(-2, -8, 39))(f) }
    assertResult(Success(List(42))) { traverse(List(21))(f) }
    assertResult(Success(List(42, 12, 78))) { traverse(List(21, 6, 39))(f) }
  }

  "Exercise 9 - sequence(list_of_partials)" should "pass this tests" in {
  }
}
