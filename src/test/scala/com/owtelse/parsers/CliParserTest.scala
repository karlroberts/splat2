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
  def recogniseShortArgFlagNames = clips.recogniseShortArgFlagNames

  def parseShortFlags = clips.parseShorFlags
  def parseLongFlags = clips.parseLongFlags
  def parseShortArgFlags = clips.parseShortArgFlags

  def oops = clips.oops

  /* enable fixtures, and actually run the tests */
  object clips {

    import parseTestHelper._

    def recogniseShortFlagNames = check { propRecogniseShortFlagNames }
    def recogniseShortArgFlagNames = check { propRecogniseShortArgFlagNames }
    def parseShorFlags = check { propParseShortFlag }

    def parseShortArgFlags = check { propParseShortArgFlag }

    def recogniseLongFlagNames =  skipped // check { propRecogniseLongFlagNames }
    def parseLongFlags =  skipped // check { propParseLongFlag }

    def oops = oops1

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

  def propKnownFlagnameParses(flagNameGen: Gen[String])(p: Parser[Any]) = forAll(flagNameGen) {
    fname: String =>
      val parseResult = parse(p)(fname)
      println("------>>> WTF parsed "+parseResult)
      parseResult match {
        case x: Success[_] => {
          println("--->> parse val =" + x.get)
          true
        }
        case _ => false
      }
  }

  def propKnownShortFlagnameParses = propKnownFlagnameParses(genShortFlagName)(shortFlagName)
  def propKnownShortArgFlagnameParses = propKnownFlagnameParses(genShortArgFlagName)(shortFlagName)

  def propKnownLongFlagnameParses = propKnownFlagnameParses(genLongFlagName)(longFlagName)


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

  def propParseShortArgFlag = propParse(genShortArgFlag)(shortFlagArg)

  def propParseLongFlag = propParse(genLongFlag)(longFlag)

  def propRecogniseShortFlagNames = propKnownShortFlagnameParses && propNotKnownShortFlagnameParses
  def propRecogniseShortArgFlagNames = propKnownShortArgFlagnameParses // && propNotKnownShortArgFlagnameParses

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
  import scalaz._
  import Scalaz._
  import org.scalacheck.{Gen, Arbitrary}
  import Arbitrary.arbitrary
  import com.owtelse.knownFlags._;

  def genShortFlagName = genFlagName(knownShortFlags.values.toSeq)
  def genLongFlagName = genFlagName(knownLongFlags.values.toSeq)
  //knownShortArgFlagName is a Map[String, (List[String] => Flag[String])]
  //ie a container of Functions... mmm sounds like an applicative..
  //genFlagName expects a Map[Sting, Flag[String]]
  // if I can use Applicative functor to apply the funcs in container to a List of Strings then I'll have a Container of
  // flags which is what I need, but Map is kind ** I need * ie M[A] not M[A,B] so a little type lambda should fix that up
  //Then I can applic it
  //do type lambda to make M[A,B] look like M[B] with A fixed.
  var sArgFlags = knownShortArgFlags.values.toList
  var theArgFlags = sArgFlags ∘ (f => f(List("dummy arg")))
  def genShortArgFlagName = genFlagName(theArgFlags)

  def genSizedNotShortFlagName(n: Int) = Gen.resize(n, genNotFlagName(knownShortFlags))
  def genSizedNotLongFlagName(n: Int) = Gen.resize(n, genNotFlagName(knownLongFlags))



  def genFlagName(knownFlagValues: Seq[Flag[String]]): Gen[String] = for {
    s <- Gen.oneOf(knownFlagValues)

  } yield { knownFlagValues.foreach(x => print(" " + x.symbol + " :")); println("----->>>> Generated known flag..." + s.symbol); s.symbol}


  def genNotFlagName(knownFlags: Map[String, Flag[String]]): Gen[String] = Gen.sized {
    size => for {
      s <- arbitrary[String]
      cleaned = s.filter{c  => val x = char2Character(c) 
      !knownFlags.contains (new String(x.toString)) && x != null }

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
  def genShortArgFlag = genFlag(flagPrefix)(genShortArgFlagName)
  
  def genFlag(flagPrefix: String)(flagNameGen: Gen[String]) = for {
    name <- flagNameGen
    flag = flagPrefix ++ name
  } yield flag



}
