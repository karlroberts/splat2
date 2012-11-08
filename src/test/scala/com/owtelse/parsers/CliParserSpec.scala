package com.owtelse.parsers

import org.specs2.{ScalaCheck, Specification}
import org.specs2.matcher.ThrownExpectations
import org.scalacheck.Prop._


/**
 * Scalacheck enabled parser test. NB Sequential as Scala parser combinator is not thread safe!!
 * TODO migrate to Scalaz parser.
 */
class CliParserSpec extends CliParserTest {
  def is = sequential ^
    "Can Recognise small flagnames but fail for unrecognised flagnames"                                                 ! recogniseShortFlagNames ^
    "Can Recognise long flagnames but fail for unrecognised flagnames"                                                  ! recogniseLongFlagNames ^
    "Can parse shortFlags"                                                                                              ! parseShortFlags ^
    "Can parse longFlags"                                                                                               ! parseLongFlags ^
    "oops"                                                                                                              ! oops ^
                                                                                                                        end
}
