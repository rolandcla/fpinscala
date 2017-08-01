package fpinscala.gettingstarted

import org.junit.Test
import junit.framework.TestCase
import org.junit.Assert._

class MyModuleTests extends TestCase {
  def testAbs {
    assertEquals(MyModule.abs(-1), 1)
  }
}