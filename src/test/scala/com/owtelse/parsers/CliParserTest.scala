package com.owtelse.parsers

import org.specs2.{ScalaCheck, Specification}
import org.specs2.matcher.ThrownExpectations
import java.lang.String
import org.scalacheck.Gen


/**
 * Created by IntelliJ IDEA.
 * User: robertk
 */

trait CliParserTest extends Specification with ScalaCheck with ThrownExpectations {

  def recogniseShortFlagNames = clips.recogniseShortFlagNames
  def recogniseLongFlagNames = clips.recogniseLongFlagNames

  def parseShortFlags = clips.parseShorFlags
  def parseLongFlags = clips.parseLongFlags

  def oops = clips.oops

  /* enable fixtures, and actually run the tests */
  object clips {

    import parseTestHelper._

    def recogniseShortFlagNames = check { propRecogniseShortFlagNames }
    def parseShorFlags = check { propParseShortFlag }

    def recogniseLongFlagNames = check { propRecogniseLongFlagNames }
    def parseLongFlags = check { propParseLongFlag }

    def oops =  oops1

  }

}

trait CliParserFixture {

  import com.owtelse.knownFlags._;

  val flagPrefix = "-"
  //val knownShortFlags = Set("p", "t", "d")
  //val knownLongFlags = Set("lax")
}

object parseTestHelper extends CLIParser with FlagGenerator {
  def parse(p: Parser[Any])(i: String) = {
    parseAll(p, i)
  }

  import org.scalacheck.Prop._
  import com.owtelse.knownFlags._;

  //oops list of specific chars I try to fail the parse with.
  def oops1 = {
    val supplimentaryCharString = new String(Character.toChars(0x495f))
    parse(longFlagName)(supplimentaryCharString) match {
      case x: Failure => {
      // println("--- OOoooPS good parse fname(" + supplimentaryCharString.size + "):"+supplimentaryCharString + " :- " + stringCodePointschars(supplimentaryCharString))
        !knownShortFlags.contains(supplimentaryCharString)
      }
      case _ => {
        println("--- OOoooPS  fname(\"+fname.size+\"):"+supplimentaryCharString + " :- " + stringCodePointschars(supplimentaryCharString))
        false
      }
    }
  }

  def propKnownFlagnameParses(knownFlags:Map[String,Flag], flagNameGen: Gen[String])(p: Parser[Any]) = forAll(flagNameGen) {
    fname: String =>
      assert(knownFlags.contains(fname), "WTF in "+knownFlags+"!!! fname is "+fname)
      val parseResult = parse(p)(fname)
      parseResult match {
        case x: Success[_] => {
          knownFlags.contains(fname)
        }
        case _ => false
      }
  }

  def propKnownShortFlagnameParses = propKnownFlagnameParses(knownShortFlags,genShortFlagName)(shortFlagName)

  def propKnownLongFlagnameParses = propKnownFlagnameParses(knownLongFlags,genLongFlagName)(longFlagName)


  //limit the generated arbitrary non flag strings to 2Chars, ie up to and bigger than known strings but not so big as to waste time generating
  def propNotKnownShortFlagnameParses = forAll(genSizedNotShortFlagName(2)) {
    fname: String =>
      parse(shortFlagName)(fname) match {
        case x: Failure => {
        //println("--- good, parse fail fname("+fname.size+"):"+fname + " :- " + stringCodePointschars(fname))
          !knownShortFlags.contains(fname)
        }
        case _ => {
          println("--- Aaarh propNotKnownFlagnameParses fname(" + fname.size + "):"+fname + " :- " + stringCodePointschars(fname))
          false
        } 
      }
  }



  //limit the generated arbitrary non flag strings to 2Chars, ie up to and bigger than known strings but not so big as to waste time generating
  def propNotKnownLongFlagnameParses = forAll(genSizedNotLongFlagName(3)) {
    fname: String =>
      parse(longFlagName)(fname) match {
        case x: Failure => {
         // println("--- good, parse fail fname("+fname.size+"):"+fname + " :- " + stringCodePointschars(fname))
          !knownLongFlags.contains(fname)
        }
        case _ => {
          println("--- Aaarh propNotKnownLongFlagnameParses fname("+ fname.size +"):"+fname + " :- " + stringCodePointschars(fname))
          false
        }
      }
  }

  def propParseShortFlag = propParse(genShortFlag)(shortFlag)

  def propParseLongFlag = propParse(genLongFlag)(longFlag)

  def propRecogniseShortFlagNames = propKnownShortFlagnameParses && propNotKnownShortFlagnameParses

  def propRecogniseLongFlagNames = propKnownLongFlagnameParses && propNotKnownLongFlagnameParses


  /**
   * Simple test for parse Success or fail
   */
  def propParse[T](flagGen: Gen[String])(p: Parser[T]) = forAll(flagGen) {
    flag: String =>
      parse(p)(flag) match {
        case _: Success[_] => true
        case _ => false
      }
  }

  // debug function. show me the unicode of chars
  def stringCodePointschars(s: String): String = {
    val ret = Predef.augmentString(s).flatMap{ c =>
      val codepoint = Character.codePointAt(Array(c),0)
      val cs: Array[Char] = Character.toChars(codepoint)
      cs match {
        case Array(one)  => cs.map(c => "\\u%s ".format(one.toInt.toHexString))
        case _ => " what the fek?---->" + cs + "<----"
      }
    }
    ret.mkString
  }
  
    
  
}

/**
 * Generates arbitrary Flagnames and !Flagnames
 */
trait FlagNameGen extends CliParserFixture {

  import org.scalacheck.{Gen, Arbitrary}
  import Arbitrary.arbitrary
  import com.owtelse.knownFlags._;

  def genShortFlagName = genFlagName(knownShortFlags)
  def genLongFlagName = genFlagName(knownLongFlags)

  def genSizedNotShortFlagName(n: Int) = Gen.resize(n, genNotFlagName(knownShortFlags))
  def genSizedNotLongFlagName(n: Int) = Gen.resize(n, genNotFlagName(knownLongFlags))



  def genFlagName(knownFlags: Map[String, Flag[String]]): Gen[String] = for {
    s <- Gen.oneOf(knownFlags.values.toSeq)
  } yield s.symbol


  def genNotFlagName(knownFlags: Set[String]): Gen[String] = Gen.sized {
    size => for {
      s <- arbitrary[String]
      cleaned = s.filter{c  => val x = char2Character(c) 
      !knownFlags.contains(new String(x.toString)) && x != null }

    } yield cleaned
  }
}

/**
 * Generates arbitrary Flags
 */
trait FlagGenerator extends FlagNameGen {
  import org.scalacheck.Gen


  def genLongFlag = genFlag(flagPrefix+flagPrefix)(genLongFlagName)
  def genShortFlag = genFlag(flagPrefix)(genShortFlagName)
  
  def genFlag(flagPrefix: String)(flagNameGen: Gen[String]) = for {
    name <- flagNameGen
    flag = flagPrefix ++ name
  } yield flag



}
