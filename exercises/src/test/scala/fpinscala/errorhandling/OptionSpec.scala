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
}
