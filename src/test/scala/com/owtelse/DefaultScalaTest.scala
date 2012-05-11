package com.owtelse

import org.specs2._
import parsers.{Flag, emptyFlag, SimpleArgFlag, CLIParser}

class CLIParserManualSpec extends Specification {
  def is = "This is a specification to manulaly check individual parsers from the bottom up"                            ^ p ^
    "The 'CLIParser'  should"                                                                                           ^
    "parse -p into type (String => SimpleArgFlag[_])"                                      ! e1 ^
    "parse -x emptyFlag" ! e2 ^
    "parse -unknown into  Failure type"                                                 ! e3 ^
                                                                                                                        end

  def e1 = clips.e1
  def e2 = clips.e2
  def e3 = clips.e3

  object clips {
    import parseTestHelper._
    def e1 = {
        val pRez = parse(shortFlag)("-p")
        pRez match {
          case x:Success[_] => {
            val pVal = x.get
            pVal match {
              case v: (String => SimpleArgFlag[_]) => true
              case _ => false
            }
          }
          case _:Failure => false
        }
    }

    def e2 = {
      val pRez = parse(shortFlag)("-x")
      pRez match {
        case x:Success[_] => {
          val pVal = x.get
          pVal match {
            case x:(String => Flag[_]) if(x.apply("whatever") == emptyFlag) =>  true
            case _ => false
          }
        }
        case _:Failure => false
      }
    }

    def e3 = {
      val pRez = parse(shortFlag)("-unknown")

      pRez match {
        case x:Success[_] => false
        case oops:Failure => { println("Ooops ----->" + oops)
          true
        }
      }
    }
  }
}

object parseTestHelper extends CLIParser {
  def parse(p: Parser[Any])(i: String) = {
    parseAll(p, i)
  }
}