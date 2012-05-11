package com.owtelse.parsers

import org.specs2.{ScalaCheck, Specification}
import org.specs2.matcher.ThrownExpectations
import org.scalacheck.Prop._


/**
 * Manual CLIParser test, todo replace with auto scalacheck
 */
class CLIParserSpec extends CLIParserTest {
  def is =
    "Can Recognise small flagnames but fail for unrecognised flagnames"                                                  ! recogniseShortFlagNames ^
    "Can Recognise long flagnames but fail for unrecognised flagnames"                                                  ! recogniseLongFlagNames ^
    "Can parse shortFlags"                                                                                                 ! parseShortFlags ^
    "Can parse longFlags"                                                                                                 ! parseLongFlags ^
    "oops"                                                                                      ! oops ^
      end
}

class CLIParserUnitTests  extends Specification { def is =

"This is a specification to check specific command lines parse correctly"                                               ^
                                                                                                                        p^
"The 'Hello world' string should"                                                                                       ^
"contain 11 characters"                                                                                                 ! e1^
"start with 'Hello'"                                                                                                    ! e2^
"end with 'world'"                                                                                                      ! e3^
end
def e1 = "Hello world" must have size(11)
def e2 = "Hello world" must startWith("Hello")
def e3 = "Hello world" must endWith("world")
}


