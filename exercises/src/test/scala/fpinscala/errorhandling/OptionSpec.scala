package fpinscala.errorhandling

import org.scalatest._
import Option._

class OptionSpec extends FlatSpec {
  "Exercise 1 - option.map(f)" should "apply f to the content of this option" in {
    assertResult(None) { None.map(x => x) }
    assertResult(Some(42)) { Some(21).map(_ * 2) }
  }

  "Exercise 1 - None.getOrElse(default)" should "return default" in {
    assertResult(42) { None.getOrElse(42) }
  }
  "Exercise 1 - Some(x).getOrElse(default)" should "return x" in {
    assertResult("brol") { Some("brol").getOrElse("paf!!!") }
  }

  "Exercise 1 - None.flatMap(f)" should "return None" in {
    assertResult(None){ None.flatMap(x => Some(x) ) }
  }

  "Exercise 1 - Some(x).flatmap(f)" should "return f(x)" in {
    assertResult(Some(42)){ Some(21).flatMap( x => Some(x * 2) ) }
    assertResult(None){ Some(21).flatMap( x => None ) }
  }

  "Exercise 1 - None.orElse(ob)" should "return ob" in {
    assertResult(Some(42)){ None orElse Some(42) }
    assertResult(None)    { None orElse None }
  }
  "Exercise 1 - Some(x).orElse(ob)" should "return Some(x)" in {
    assertResult(Some(42)){ Some(42) orElse Some(24) }
    assertResult(Some(43)){ Some(43) orElse None }
  }

  "Exercise 1 - None.filter(p)" should "return None" in {
    assertResult(None){ None.filter(x => true) }
    assertResult(None){ None.filter(x => false) }
  }

  "Exercise 1 - Some(x).filter(p)" should "return Some(x) if p(x) is true else None" in {
    assertResult(Some(42)){ Some(42).filter(_ > 10) }
    assertResult(None)    { Some(42).filter(_ > 100) }
  }

  "Exercise 2 - variance(xs)" should "return None if xs is empty" in {
    assertResult(None) { variance(List()) }
  }

  "Exercise 2 - variance(xs)" should "return Some(0) if all elements of xs are equal" in {
    assertResult(Some(0.0)) { variance(List(42, 42, 42))}
  }

  "Exercise 2 - variance(xs)" should "do his work ;)" in {
    assertResult(Some(1.0)) { variance(Vector(4, 6)) }
    assertResult(Some(141.6875)) { variance(Vector(12,32,45,34)) }
  }

  "Exercise 3 - map2(None, _opt_)(_f_)" should "return None" in {
    assertResult(None){ map2(None, Some(42))((x: Int, y: Int) => (x + y).toString) }
    assertResult(None){ map2(None, None)    ((x: Int, y: Int) => (x + y).toString) }
  }

  "Exercise 3 - map2(_opt_, None)(_f_)" should "return None" in {
    assertResult(None){ map2(Some(42), None)((x: Int, y: Int) => (x + y).toString) }
  }

  "Exercise 3 - map2(Some(x), Some(y))(f)" should "return Some(f(x,y))" in {
    assertResult(Some("59")){ map2(Some(42), Some(17))((x: Int, y: Int) => (x + y).toString) }
    assertResult(Some(List(42,1,2,3))){ map2(Some(List(1,2,3)), Some(42))((l, x) => x :: l) }
  }

  "Exercise 4 - bothMatch_2(\"12.?\", \"\\d\\dx\")(\"12x\")" should "return Some(true)" in {
    assertResult(Some(true)){ bothMatch_2("12.?", "\\d\\dx", "12x") }
  }

  "Exercise 4 - bothMatch_2(\"12.?\", \"\\d\\dx\")(\"12y\")" should "return Some(false)" in {
    assertResult(Some(false)){ bothMatch_2("12.?", "\\d\\dx", "12y") }
  }

  "Exercise 4 - bothMatch_2(\")12.?\", \"\\d\\dx(\")(\"12y\"" should "return None (compilation error in 1st arg)" in {
    assertResult(None){ bothMatch_2(")12.?", "\\d\\dx", "12y") }
  }

  "Exercise 4 - bothMatch_2(\"12.?\", \"\\d\\dx(\")(\"12y\")" should "return None (compilation error in 2nd arg)" in {
    assertResult(None){ bothMatch_2("12.?", "\\d\\dx(", "12y") }
  }

  "Exercise 5 - sequence(List())" should "return Some(List())" in {
    assertResult(Some(List())){ sequence(List()) }
  }

  "Exercise 5 - sequence(list_of_options)" should "return None if list_of_options contains None" in {
    assertResult(None){ sequence(List(None)) }
    assertResult(None){ sequence(List(None, Some(12), Some(21))) }
    assertResult(None){ sequence(List(Some(12), Some(21), None)) }
    assertResult(None){ sequence(List(Some(12), None, Some(21), Some(78))) }
  }

  "Exercise 5 - sequence(list_of_options)" should "return Some(List(values))" in {
    assertResult(Some(List(1,2,3))) {sequence(List(Some(1), Some(2), Some(3)))}
  }

  "Exercise 6 - traverse(List())" should "return Some(List())" in {
    assertResult(Some(List())){ traverse(List())(x => x) }
  }

  "Exercise 6 - traverse(list)(f)" should "return None if ∃x, x ∈ list ∧ f(x) = None" in {
    def f(x: Int): Option[String] = if (x > 0) Some((2 * x) toString) else None

    assertResult(None){ traverse (List(-1)) (f) }
    assertResult(None){ traverse (List(-1, 2, 3)) (f) }
    assertResult(None){ traverse (List(3, 4, -1)) (f) }
    assertResult(None){ traverse (List(3, 4, -1, 5, 6)) (f) }
  }

  "Exercice 6 - traverse(list)(f))" should "return Some(List(f(x),...)) ∀x ∈ list" in {
    def f(x: Int): Option[String] = if (x > 0) Some((2 * x) toString) else None

    assertResult(Some(List("42"))){ traverse (List(21)) (f) }
    assertResult(Some(List("42", "78", "2222"))){ traverse (List(21, 39, 1111)) (f) }
  }

  "Exercice 6 - sequence2 (based on traverse)" should "work like sequence" in {
    assertResult(Some(List())){ sequence2(List()) }

    assertResult(None){ sequence2(List(None)) }
    assertResult(None){ sequence2(List(None, Some(12), Some(21))) }
    assertResult(None){ sequence2(List(Some(12), Some(21), None)) }
    assertResult(None){ sequence2(List(Some(12), None, Some(21), Some(78))) }

    assertResult(Some(List(1,2,3))) {sequence2(List(Some(1), Some(2), Some(3)))}
  }
}
