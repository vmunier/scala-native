package scala

import org.junit.Assert._

object ShortSuite extends tests.Suite {
  test("should_always_be_in_their_range") {
    def test(x: Int, y: Short): Unit =
      assertEquals(y, x.toShort)

    test(0, 0)
    test(-500, -500)
    test(-90000, -24464)
    test(123456789, -13035)
    test(-40000, 25536)
    test(65536, 0)
    test(32768, -32768)

    def testC(x: Char, y: Short): Unit =
      assertEquals(x.toShort, y)

    testC(-1.toChar, -1)
    testC(200.toChar, 200)
    testC(60000.toChar, -5536)
  }
}
