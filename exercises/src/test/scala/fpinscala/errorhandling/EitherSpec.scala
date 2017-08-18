package fpinscala.errorhandling

import org.scalatest._
import Either._

class EitherSpec extends FlatSpec {
  "Exercise 7 - either.map(f)" should "apply f to the content of this 'either'" in {
    assertResult(Left("Error!")) { Left("Error!").map(x => x) }
    assertResult(Right(42)) { Right(21).map(_ * 2) }
    assertResult(Right("42")) { Right(21).map(x => (x * 2).toString) }
  }

  "Exercice 7 - Left(x).flatMap(f)" should "return Left(x)" in {
    assertResult(Left("Error!")) { Left("Error!") flatMap {x => x} }
  }

  "Exercise 7 - Right(x).flatMap(f)" should "return f(x)" in {
    def div10(x: Int): Either[String, Int] =
      try { Right(10 / x) } catch {case e: Exception => Left("Division by zero !")}

    assertResult(Right(5)) { Right(2).flatMap(div10) }
    assertResult(Left("Division by zero !")) { Right(0).flatMap(div10) }
  }

  "Exercise 7 - Left(x).orElse(e)" should "return e" in {
    assertResult(Right(42))      {Left("Brol") orElse Right(42) }
    assertResult(Left("Rebrol")) {Left(78) orElse Left("Rebrol") }
  }

  "Exercise 7 - Right(x).orElse(e)" should "return Right(x)" in {
    assertResult(Right(42)) { Right(42) orElse Left("Error!") }
    assertResult(Right(42)) { Right(42) orElse Right(42.42) }
  }

  "Exercise 7 - Left(x).map2(_either_)(f)" should "return Left(x)" in {
    assertResult(Left("Brol")) { Left("Brol").map2 (Left("Coucou")) {(_, _) => 42} }
    assertResult(Left("Brol")) { Left("Brol").map2 (Right("Coucou")) {(_, _) => 42} }
  }

  "Exercise 7 - _either_.map2(Left(x))(f)" should "return Left(x)" in {
    assertResult(Left("Coucou")) { Right("Brol").map2 (Left("Coucou")) {(_, _) => 42} }
  }

  "Exercise 7 - Right(x) map2 (Right(y)) (f)" should "return Right(f(x,y))" in {
    assertResult(Right("Brol42")) { Right("Brol").map2 (Right(42)) (_ + _.toString) }
  }

  "Exercice 8 - traverse(List())(f)" should "return Right(List())" in {
    assertResult(Right(List())) { traverse(List())(x => x) }
  }

  "Exercise 8 - traverse(list)(f)" should "return the 1st Left(e) returned by succesive f(x) for x ∈ list" in {
    def f(x: Int): Either[String, Int] = if (x > 0) Right(2 * x) else Left(x.toString + " <= 0")

    assertResult(Left("-2 <= 0")) { traverse(List(21, -2, 39))(f) }
    assertResult(Left("-2 <= 0")) { traverse(List(21, 39, -2))(f) }
    assertResult(Left("-2 <= 0")) { traverse(List(-2, -8, 39))(f) }
  }

  "Exercise 8 - traverse(list)(f)" should "return Right(List(f(x)...))) if there is no Left returned by f" in {
    def f(x: Int): Either[String, Int] = if (x > 0) Right(2 * x) else Left(x.toString + " <= 0")

    assertResult(Right(List(42))) { traverse(List(21))(f) }
    assertResult(Right(List(42, 12, 78))) { traverse(List(21, 6, 39))(f) }
  }

  "Exercise 8 - sequence(List())" should "return Right(List())" in {
    assertResult(Right(List())){ sequence(List()) }
  }

  "Exercise 8 - sequence(list_of_eithers)" should "return the 1st Left found in list_of_eithers" in {
    assertResult(Left("Brol")){ sequence(List(Left("Brol"))) }
    assertResult(Left("Brol")){ sequence(List(Left("Brol"), Right(12), Right(21))) }
    assertResult(Left("Brol")){ sequence(List(Right(12), Right(21), Left("Brol"))) }
    assertResult(Left("Brol")){ sequence(List(Right(12), Left("Brol"), Right(21), Right(78))) }
  }

  "Exercise 8 - sequence(list_of_eithers)" should "return Right(List(values)) if ∄ Left in list_of_eithers" in {
    assertResult(Right(List(1,2,3))) {sequence(List(Right(1), Right(2), Right(3)))}
  }

}
