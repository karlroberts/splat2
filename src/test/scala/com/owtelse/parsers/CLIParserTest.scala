package com.owtelse.parsers

import org.specs2.{ScalaCheck, Specification}
import org.specs2.matcher.ThrownExpectations

/**
 * Created by IntelliJ IDEA.
 * User: robertk
 */

trait CLIParserTest extends Specification with ScalaCheck with ThrownExpectations {

  def recogniseFlagNames = clips.recogniseFlagNames

  def parseShorFlags = clips.parseShorFlags

  /* enable fixtures, and actually run the tests */
  object clips {

    import parseTestHelper._

    def recogniseFlagNames = check { propRecogniseFlagNames }

    def parseShorFlags = check { propParseShortFlag }

  }

}

trait CLIParserFixtures {

  import collection.immutable.SortedSet

  val flagPrefix = "-"
  val knownShortFlags = SortedSet("p", "t", "d")
  val knownLongFlags = Set("lax")
}


object parseTestHelper extends CLIParser with shortFlagGenerator {
  def parse(p: Parser[Any])(i: String) = {
    parseAll(p, i)
  }

  import org.scalacheck.Prop._

  val propKnownFlagnameParses = forAll(genFlagName) {
    fname: String =>
      val parseResult = parse(shortFlagName)(fname)
      parseResult match {
        case x: Success[_] => knownShortFlags.contains(fname)
        case _ => false
      }
  }

  //limit the generated arbitrary non flag strings to 2Chars, ie up to and bigger than known strings but not so big as to waste time generating
  val propNotKnownFlagnameParses = forAll(genSizedNotFlagName(2)) {
    fname: String =>
    //println("not flag:- " + fname)
      parse(shortFlagName)(fname) match {
        case x: Failure => !knownShortFlags.contains(fname)
        case _ => false
      }
  }

  val propParseShortFlag = forAll(genShortFlag) {
    flag: String =>
      parse(shortFlag)(flag) match {
        case _: Success[_] => true
        case _ => false
      }
  }

  val propRecogniseFlagNames = propKnownFlagnameParses && propNotKnownFlagnameParses
}

/**
 * supplies methods to generate arbit
 */
trait FlagNameGen extends CLIParserFixtures {

  import org.scalacheck.{Gen, Arbitrary}
  import Arbitrary.arbitrary

  def genFlagName: Gen[String] = for {
    s <- Gen.oneOf(knownShortFlags.toSeq)
  } yield s

  def genNotFlagName: Gen[String] = Gen.sized {
    size => for {
      s <- arbitrary[String]
      res = if (knownShortFlags.contains(s)) Gen.fail else s
    } yield s
  }

  def genSizedNotFlagName(n: Int) = Gen.resize(n, genNotFlagName)

}

trait shortFlagGenerator extends FlagNameGen {
  def genShortFlag = for {
    name <- genFlagName
    flag = flagPrefix ++ name
  } yield flag
}
