package fpinscala.gettingstarted

import org.junit.Test
import junit.framework.TestCase
import org.junit.Assert._

class MyModuleTests extends TestCase {
  def testAbs {
    import MyModule.abs
    assertEquals(abs(-1), 1)
    assertEquals(abs(12), 12)
    assertEquals(abs(0), 0)
  }

  // Test for Exercise 1
  def testFib {
    import MyModule.fib
    assertEquals(fib(0), 0)
    assertEquals(fib(1), 1)
    assertEquals(fib(2), 1)
    assertEquals(fib(3), 2)
    assertEquals(fib(6), 8)
    assertEquals(fib(9), 34)
    assertEquals(fib(11), 89)
  }
}

class PolymorphicFunctionsTests extends TestCase {
  def testBinarySearch {
    import PolymorphicFunctions.binarySearch
    assertEquals(binarySearch(Array(3,4,5,6,7), 6, (x: Int , y: Int) => x > y), 3)
  }

  // Test for Exercise 2
  def testIsSorted {
    import PolymorphicFunctions.isSorted
    assertTrue(isSorted(Array(), (x: Int , y: Int) => x > y))
    assertTrue(isSorted(Array(42), (x: Int , y: Int) => x > y))
    assertTrue(isSorted(Array(1,2,3,4,12,42), (x: Int , y: Int) => x > y))
    assertTrue(isSorted(Array(1,2,2,3,3,41,41), (x: Int , y: Int) => x > y))
    assertFalse(isSorted(Array(1,2,3,2,4), (x: Int , y: Int) => x > y))
    assertFalse(isSorted(Array(1,2,3,4,3), (x: Int , y: Int) => x > y))
    assertFalse(isSorted(Array(3,1,2,3,4), (x: Int , y: Int) => x > y))
  }

  // Test for Exercise 3
  def testCurry {
    import PolymorphicFunctions.curry
    var f = curry(math.max)
    var g = f(42)
    assertEquals(g(12), 42)
    assertEquals(g(52), 52)
  }

  // Test for Exercise 4
  def testUncurry {
    import PolymorphicFunctions.uncurry
    def cmax(a: Int) = (b: Int) => math.max(a, b)
    assertEquals(cmax(12)(42), 42)
    var my_max = uncurry(cmax)
    assertEquals(my_max(12,42), 42)
    assertEquals(my_max(52,42), 52)
  }

  def testCompose {
    import PolymorphicFunctions.compose
    def inc(x: Int) = x + 1
    def str(x: Int) = x.toString()

    val f = compose(str, inc)
    assertEquals(f(42), "43")
  }
}