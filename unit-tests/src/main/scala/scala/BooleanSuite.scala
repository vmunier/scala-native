package scala

import org.junit.Assert._

object BooleanSuite extends tests.Suite {
  test("primitive_operations_on_booleans_should_return_correct_results") {
    assertFalse(false & false)
    assertFalse(false & true)
    assertFalse(true & false)
    assertTrue(true & true)

    assertFalse(false | false)
    assertTrue(true | false)
    assertTrue(false | true)
    assertTrue(true | true)

    assertFalse(false ^ false)
    assertTrue(true ^ false)
    assertTrue(false ^ true)
    assertFalse(true ^ true)
  }
}
