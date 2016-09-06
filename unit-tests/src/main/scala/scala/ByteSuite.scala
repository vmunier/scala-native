package scala

import org.junit.Assert._

object ByteSuite extends tests.Suite {
  test("should_always_be_in_their_range") {
    def test(x: Int, y: Byte): Unit =
      assertEquals(y, x.toByte)

    test(0, 0)
    test(127, 127)
    test(128, -128)
    test(-128, -128)
    test(-500, 12)
    test(-90000, 112)
    test(123456789, 21)
    test(-40000, -64)
    test(65536, 0)
    test(32768, 0)

    def testC(x: Char, y: Byte): Unit =
      assertEquals(y, x.toByte)

    testC(-1.toChar, -1)
    testC(200.toChar, -56)
  }
}
