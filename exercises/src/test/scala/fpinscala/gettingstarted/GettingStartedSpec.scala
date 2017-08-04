package fpinscala.gettingstarted

import org.scalatest._

class MyModuleSpec extends FlatSpec {
  import MyModule._

  "abs(x)" should "return the absolute value of x" in {
    assert(abs(-1) == 1)
    assert(abs(12) == 12)
    assert(abs(0) == 0)
  }
  "fib(n)" should "return the nth Fibonacci number" in {
    assert(fib(0) == 0)
    assert(fib(1) == 1)
    assert(fib(2) == 1)
    assert(fib(3) == 2)
    assert(fib(6) == 8)
    assert(fib(9) == 34)
    assert(fib(11) == 89)
  }
}

class PolymorphicFunctionsSpec extends FlatSpec {
  import PolymorphicFunctions._

  "binarySearch(xs, y, cmp_fn)" should "return the index of y in the sorted Array xs" in {
    assert(binarySearch(Array(3,4,5,6,7), 6, (x: Int , y: Int) => x > y) == 3)
  }
  "isSorted(xs, cmp_fn)" should "checks whether an xs is sorted according to cmp_fn" in {
    assert(isSorted(Array(), (x: Int , y: Int) => x > y))
    assert(isSorted(Array(42), (x: Int , y: Int) => x > y))
    assert(isSorted(Array(1,2,3,4,12,42), (x: Int , y: Int) => x > y))
    assert(isSorted(Array(1,2,2,3,3,41,41), (x: Int , y: Int) => x > y))
    assert(! isSorted(Array(1,2,3,2,4), (x: Int , y: Int) => x > y))
    assert(! isSorted(Array(1,2,3,4,3), (x: Int , y: Int) => x > y))
    assert(! isSorted(Array(3,1,2,3,4), (x: Int , y: Int) => x > y))
  }
  "curry(f)" should "return the curried function" in {
    var f = curry(math.max)
    var g = f(42)
    assert(g(12) == 42)
    assert(g(52) == 52)
  }
  "uncurry(f)" should "return the uncurried function" in {
    def cmax(a: Int) = (b: Int) => math.max(a, b)
    assert(cmax(12)(42) == 42)
    var my_max = uncurry(cmax)
    assert(my_max(12,42) == 42)
    assert(my_max(52,42) == 52)
  }
  "compose(f, g)" should "return the composed function: f(g(_))" in {
    def inc(x: Int) = x + 1
    def str(x: Int) = x.toString()

    val f = compose(str, inc)
    assert(f(42) == "43")
  }
}
