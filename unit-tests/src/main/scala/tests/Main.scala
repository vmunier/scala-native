package tests

import java.lang.System.exit

object Main {
  val suites = Seq[Suite](
      java.lang.FloatSuite,
      java.lang.DoubleSuite,
      java.util.RandomSuite,
      scala.scalanative.native.CStringSuite,
      scala.BooleanSuite,
      scala.ByteSuite,
      scala.CharSuite,
      scala.DoubleSuite,
      scala.FloatSuite,
      scala.IntSuite,
      //scala.LongSuite,
      scala.MatchSuite,
      scala.ShortSuite,
      scala.UnitSuite,
      tests.SuiteSuite
  )

  def main(args: Array[String]): Unit = {
    if (!suites.forall(_.run)) exit(1) else exit(0)
  }
}