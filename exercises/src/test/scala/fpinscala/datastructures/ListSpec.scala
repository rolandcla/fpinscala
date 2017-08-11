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

  "Exercise 7 - product using foldRight" can "not be implemented using short-circuit for any zero in the list" in {
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

  "Exercise 10 - foldLeft(xs, z)(f)" should "reduce l by successive call to f(z,x) -> z" in {
    assertResult(6){ foldLeft(List(1,2,3), 0)(_ + _) }
    assertResult(0){ foldLeft(List(), 0)((acc: Int, x: Int) => acc + x) }
    assertResult(List(3,2,1)) { foldLeft[Int, List[Int]](List(1,2,3), Nil)((xs, x) => Cons(x, xs)) }
  }

  "Exercise 11 - sumFL(xs)" should "return the sum of xs" in {
    assertResult(6){ sumFL(List(1,2,3)) }
  }
  it should "return 0 for an empty list" in {
    assertResult(0){ sumFL(Nil) }
  }

  "Exercise 11 - productFL(xs)" should "return the product of xs" in {
    assertResult(24.0){ productFL(List(1.0,2.0,3.0,4.0)) }
    assertResult(0.0){ productFL(List(1.0,2.0,3.0,4.0,0.0,5.0,6.0,7.0)) }
  }
  it should "return 1 for an empty list" in {
    assertResult(1){ productFL(Nil) }
  }

  "Exercise 11 - lengthFL(xs)" should "return the length of xs" in {
    assertResult(5){ lengthFL(List(1, 2.0, "brol", List(1,2,3), 5)) }
    assertResult(0){ lengthFL(Nil) }
  }

  "Exercise 12 - reverse(xs)" should "return the elements of xs in reversed order" in {
    assertResult(List(5,4,3,2,1)){ reverse(List(1,2,3,4,5)) }
    assertResult(Nil) { reverse(List()) }
    val xs = List(1, 2.0, "brol", List(1,2,3), 5)
    assertResult(xs) { reverse(reverse(xs)) }
  }

  "Exercise 13 - foldRightUsingFL" should "Work as the original foldRight" in {
    assertResult( foldRight(List(1,2,3), 0)(_ + _) ) { foldRightUsingFL(List(1,2,3), 0)(_ + _) }
    assertResult( List(1,2,3) ) { foldRightUsingFL[Int, List[Int]](List(1,2,3), Nil)( (x,acc) => Cons(x, acc)) }
  }

  "Exercise 13 - foldLeftUsingFR" should "Work as the original foldLeft" in {
    assertResult( foldLeft(List(1,2,3), 0)(_ + _) ) { foldLeftUsingFR(List(1,2,3), 0)(_ + _) }
    assertResult( List(3,2,1) ) { foldLeftUsingFR[Int, List[Int]](List(1,2,3), Nil)( (acc,x) => Cons(x, acc)) }
  }

  "Exercise 14 - appendUsingFR" should "Work as the original append" in {
    assertResult(List(1, 2.0, "azerty", List(3,5,6))) {
      appendUsingFR(List(1, 2.0), List("azerty", List(3,5,6))) }
    assertResult(append(List(1,2,3), List(4,6,6))) {
      appendUsingFR(List(1,2,3), List(4,6,6)) }
    assertResult(append(List(1,2,3), Nil)) { appendUsingFR(List(1,2,3), Nil) }
    assertResult(append(Nil, List(1,2,3))) { appendUsingFR(Nil, List(1,2,3)) }
  }

  "Exercise 15 - concatenate" should "concatenate a list of list into a single list" in {
    assertResult(List(1,2,3,4,5,6,7,8)) {
      concatenate(List(List(1,2), List(3,4,5), Nil, List(6,7), List(8)))
    }
  }
  it should "return Nil for an empty list and for a List of empty lists" in {
    assertResult(Nil)(concatenate(Nil))
    assertResult(Nil)(concatenate(List(Nil, Nil, Nil)))
  }

  "Exercise 16 - addOne(xs)" should "add 1 to each element of xs" in {
    assertResult(Nil){ addOne(Nil) }
    assertResult(List(2,3,4)){ addOne(List(1,2,3)) }
  }

  "Exercise 17 - mapDblToStr(ds)" should "turn each Double in ds to a String" in {
    assertResult(Nil){ mapDblToStr(Nil) }
    val ds = List(2.0, 4.2, 0.42)
    assertResult(List("2.0", "4.2", "0.42")){ mapDblToStr(ds) }
  }

  "Exercise 18 - map(xs)(f)" should "return a list of each element of xs applied to f" in {
    assertResult(Nil){ map(Nil: List[Int])(_ + 1)}
    assertResult(List(2,3,4)){ map(List(1,2,3))(_ + 1) }
    val ds = List(2.0, 4.2, 0.42)
    assertResult(List("2.0", "4.2", "0.42")){ map(ds)(d => d.toString) }
  }

  "Exercise 19 - filter(xs)(p)" should "remove each element of xs unless they satisfy the predicate p" in {
    assertResult(Nil){ filter(Nil: List[Int])(x => true) }
    assertResult(List(1,2,3)) {filter(List(1,2,3))(x => true)}
    assertResult(List(1,2,3)) {filter(List(-4,1,-2,2,3,-5))(_ > 0)}
  }

  "Exercise 20 - flatMap(xs)(f)" should "work like map but with the lists produced by f concatenated to the result" in {
    val xs = List(1,2,3)
    assertResult(Nil){ flatMap(Nil: List[Int])(x => List(x)) }
    assertResult(List(1,2,3)) { flatMap(xs)(x => List(x))}
    assertResult(List(1,2,2,4,3,6)) { flatMap(xs)(x => List(x, x*2))}
  }

  "Exercise 21 - filterFM(xs)(p)" should "work like filter" in {
    assertResult(Nil){ filterFM(Nil: List[Int])(x => true) }
    assertResult(List(1,2,3)) {filterFM(List(1,2,3))(x => true)}
    assertResult(List(1,2,3)) {filterFM(List(-4,1,-2,2,3,-5))(_ > 0)}
  }

  "Exercise 22 - add2List(xs, ys)" should "construct a new list by adding corresponding elements of xs and ys" in {
    assertResult(Nil) { add2List(List(1,2,3), Nil) }
    assertResult(Nil) { add2List(Nil, List(1,2,3)) }
    assertResult(List(5,7,9)) { add2List(List(4,5,6), List(1,2,3)) }
    assertResult(List(5,7,9)) { add2List(List(4,5,6,42), List(1,2,3)) }
    assertResult(List(5,7,9)) { add2List(List(4,5,6), List(1,2,3,42)) }
  }

  "Exercise 23 - zip(xs, ys)" should "construct a new list by applying f corresponding elements of xs and ys" in {
    assertResult(Nil) { zip(List(1,2,3), Nil: List[Int])(_ + _) }
    assertResult(Nil) { zip(Nil: List[Int], List(1,2,3))(_ + _) }
    assertResult(List(5,7,9)) { zip(List(4,5,6), List(1,2,3))(_ + _) }
    assertResult(List(5,7,9)) { zip(List(4,5,6,42), List(1,2,3))(_ + _) }
    assertResult(List(5,7,9)) { zip(List(4,5,6), List(1,2,3,42))(_ + _) }
  }

  "Exercise 24 - hasSubsequence(l, sub)" should "checking whether l contains sub" in {
    assertResult(true) { hasSubsequence(Nil, Nil) }
    assertResult(true) { hasSubsequence(List(1,2,3,4), Nil) }
    assertResult(true) { hasSubsequence(List(1,2,3,4), List(1)) }
    assertResult(true) { hasSubsequence(List(1,2,3,4), List(2)) }
    assertResult(true) { hasSubsequence(List(1,2,3,4), List(4)) }
    assertResult(true) { hasSubsequence(List(1,2,3,4), List(1,2)) }
    assertResult(true) { hasSubsequence(List(1,2,3,4), List(2,3)) }
    assertResult(true) { hasSubsequence(List(1,2,3,4), List(3,4)) }
    assertResult(true) { hasSubsequence(List(1,2,3,4), List(1,2,3,4)) }
    assertResult(false) { hasSubsequence(List(), List(3)) }
    assertResult(false) { hasSubsequence(List(1,2), List(3)) }
    assertResult(false) { hasSubsequence(List(1,2,3,4), List(3,2)) }
    assertResult(false) { hasSubsequence(List(1,2,3,4), List(1,2,3,4,5)) }
    assertResult(false) { hasSubsequence(List(1,2,3,4), List(0,1,2,3,4)) }
  }
}
