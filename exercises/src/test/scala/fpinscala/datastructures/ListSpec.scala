package fpinscala.datastructures

import org.scalatest._
import List._

class ListSpec extends FlatSpec {
  "Exercise 1" should "return 3" in {
    assertResult(3) {x}
  }

  "Exercise 2 - tail(xs: List)" should "remove the first element of xs" in {
    var xs = List(1, 2, 3)
    assertResult(List(2,3)){ tail(xs) }
    assertResult(Nil){ tail(List(42)) }
  }
  it should "throw NoSuchElementException on an empty List" in {
    assertThrows[NoSuchElementException] {
      tail(Nil)
    }
    assertThrows[NoSuchElementException] {
      tail(tail(List(42)))
    }
  }

  "Exercise 3 - drop(xs: List, n)" should "remove the n first elements of xs" in {
    var xs = List(1,2,3,4,5,6)
    assertResult(xs){ drop(xs, 0) }
    assertResult(Nil){ drop(xs, 6) }
    assertResult(List(5,6)){ drop(xs, 4) }
    assertResult(Nil){ drop(Nil,0) }
  }
  it should "throw NoSuchElementException if the length of xs < n" in {
    assertThrows[NoSuchElementException] {
      (drop(Nil, 1))
    }
    assertThrows[NoSuchElementException] {
      (drop(List(1,2,3), 4))
    }
  }

  "Exercise 4 - dropWhile(xs: List, f)" should "remove elements from xs prefix as long as they match f" in {
    var xs = List(1,2,3,4,5,6)
    assertResult(xs){ dropWhile(xs, (x: Int) => x < 1) }
    assertResult(Nil){ dropWhile(xs, (x: Int) => x < 12) }
    assertResult(List(3,4,5,6)){ dropWhile(xs, (x: Int) => x < 3) }
    assertResult(Nil){ dropWhile(Nil, (x: Any) => true) }
    assertResult(Nil){ dropWhile(Nil, (x: Any) => false) }
  }

  "Exercise 5 - setHead(xs: List, y)" should "replace the first elem of xs with y" in {
    assertResult(List(42,2,3)){ setHead(List(1,2,3), 42) }
    assertResult(Cons(42,Nil)){ setHead(Cons(1, Nil), 42) }
  }
  it should "throw NoSuchElementException on an empty List" in {
    assertThrows[NoSuchElementException] {
      setHead(List(), 42)
    }
  }

  "Exercise 6 - init(xs: List)" should "returns a List consisting of all but the last element of xs" in {
    assertResult(List(1,2,3)){ init(List(1,2,3,4)) }
    assertResult(Nil){ init(List(4)) }
  }
  it should "throw NoSuchElementException on an empty List" in {
    assertThrows[NoSuchElementException] {
      init(Nil)
    }
    assertThrows[NoSuchElementException] {
      init(init(List(1)))
    }
  }

  "Exercise 7 - product using foldRight" should "!!! be implemented !!!" in {
  }

  "Exercise 8 - Passing Nil and Cons to foldRight" should "return a copy of the list" in {
    val xs = List(1,2,3,4,5)
    assertResult(xs) { foldRight(xs, Nil: List[Int])(Cons(_,_)) }
    assertResult(Nil) { foldRight(Nil, Nil: List[Int])(Cons(_,_)) }
  }

  "Exercise 9 - length(xs: List)" should "return the number of element(s) in xs" in {
    assertResult(0){ length(Nil) }
    assertResult(1){ length(List(42)) }
    assertResult(4){ length(List(4.0,5,Nil,"brol")) }
  }
}
