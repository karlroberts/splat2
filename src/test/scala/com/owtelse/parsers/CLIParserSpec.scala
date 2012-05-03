package com.owtelse.parsers

import org.specs2.{ScalaCheck, Specification}
import org.specs2.matcher.ThrownExpectations
import org.scalacheck.Prop._


/**
 * Manual CLIParser test, todo replace with auto scalacheck
 */
class CLIParserSpec extends CLIParserTest {
  def is =
    "Can Recognise small flagnames but fail for unrecognised flagnames" ! recogniseFlagNames ^
    "Can parse shortFlags" ! parseShorFlags ^
      end
}


