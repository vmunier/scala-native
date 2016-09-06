package scala

import org.junit.Assert._

object UnitSuite extends tests.Suite {
  test("should_have_hashCode") {
    assertEquals(0, ().hashCode())
    assertEquals(0, ((): Any).hashCode())
    assertEquals(0, ().##)
  }

  test("should_equal_itself") {
    assertTrue(().equals(()))
    assertTrue(((): Any).equals((): Any))
  }

  test("should_not_equal_other_values") {
    def testAgainst(v: Any): Unit = {
      assertFalse(().equals(v))
      assertFalse(((): Any).equals(v))
    }

    testAgainst(0)
    testAgainst(1)
    testAgainst(null)
    testAgainst(false)
    testAgainst("")
  }
}
