package fpinscala.datastructures

import org.scalatest._
import List._

class ListSpec extends FlatSpec {
  "tail(xs: List)" should "remove the first element of xs" in {
    var xs = List(1, 2, 3)
    assert(tail(xs) == List(2,3))
    assert(tail(List(42)) == Nil)
  }
  it should "throw NoSuchElementException on an empty List" in {
    assertThrows[NoSuchElementException] {
      tail(Nil)
    }
    assertThrows[NoSuchElementException] {
      tail(tail(List(42)))
    }
  }

  "setHead(xs: List, y)" should "replace the first elem of xs with y" in {
    assert(setHead(List(1,2,3), 42) == List(42,2,3))
    assert(setHead(Cons(1, Nil), 42) == Cons(42,Nil))
  }
  it should "throw NoSuchElementException on an empty List" in {
    assertThrows[NoSuchElementException] {
      setHead(List(), 42)
    }
  }
}
