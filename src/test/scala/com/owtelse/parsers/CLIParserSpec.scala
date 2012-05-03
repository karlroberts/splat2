package com.owtelse.parsers

import org.specs2.{ScalaCheck, Specification}
import org.specs2.matcher.ThrownExpectations
import collection.immutable.SortedSet
import org.scalacheck.{Gen, Arbitrary}


/**
 * Manual CLIParser test, todo replace with auto scalacheck
 */
class CLIParserSpec extends CLIParserTest {
    def is =
      "Can Recognise small flagnames but fail for unrecognised flagnames" ! recogniseFlagNames ^
        end
  }



trait CLIParserTest  extends Specification with ScalaCheck with ThrownExpectations{

  def recogniseFlagNames = clips.recogniseFlagNames
  
  //Fixtures
  
  

  object clips  extends CLIParser with FlagNameGen {
    //import org.scalacheck._
    import org.scalacheck.Prop._

   def recogniseFlagNames = check  { propRecogniseFlagNames }

    val propKnownFlagnameParses = forAll(genFlagName) { fname: String =>
      val parseResult = parse(shortFlagName)(fname)
      val testres = parseResult match {
        case x:Success[_] => knownShortFlags.contains(fname)
        case _ => false
      }
      testres mustEqual true
    }
    
    val propRecogniseFlagNames = propKnownFlagnameParses


    def parse(p: Parser[Any])(i: String) = {
      parseAll(p, i)
    }
  }




}

trait CLIParserFixtures  {
  val knownShortFlags = SortedSet("p","t","d")
  val knownLongFlags = Set("lax")


}




trait FlagNameGen extends CLIParserFixtures {
  import org.scalacheck.{Gen, Arbitrary}
  import Arbitrary.arbitrary
  implicit def arbFlagName: Arbitrary[String] = Arbitrary { genFlagName }
  
  def genFlagName :Gen[String] = for {
    s <- Gen.oneOf(knownShortFlags.toSeq)
  } yield s

  def genNotFlagName = Gen.sized { size =>
    for {
      cList <- Gen.listOf(arbitrary[Char])
    } yield cList.mkString
  }
}
