package com.owtelse

import org.specs2._
import matcher.{MustMatchers, MustExpectations, ThrownExpectations}
import parsers.{Flag, emptyFlag, SimpleArgFlag, LongFlag,  CLIParser}

class CliParserManualSpec extends Specification {
  def is = "This is a specification to manulaly check individual parsers from the bottom up"                            ^ p ^
    "The 'CLIParser'  should"                                                                                           ^
    "parse any word into a word"                                                                                        ! aWord ^
    "parse any flagArg, ie ':' separated word into a List[String] which each arg ocupying a pos in the list"            ! checkflagArg ^
    "shortFlag will parse -p into type (String => SimpleArgFlag[_])"                                                    ! properties ^
    "shortFlag will parse -t into type (String => SimpleArgFlag[_])"                                                    ! templates ^
    "shortFlag will parse -d into type (String => SimpleArgFlag[_])"                                                    ! templateDirs ^
    "shortFlag will parse -unknown into  Failure type"                                                                  ! unknown ^
    "shortFlagArg will parse -p a1:a2:a3 into a props containing the args"                                              ! propsWithArgs ^
    "longFlag will parse --lax into a lax Flag"                                                                         ! lax ^
                                                                                                                        end

  def aWord = clips.aWord
  def checkflagArg = clips.checkflagArg
  def properties = clips.e1
  def unknown = clips.unknown
  def templates = clips.templates
  def templateDirs = clips.templateDirs
  def propsWithArgs = clips.propsWithArgs
  def lax =  clips.lax

  object clips {
    import parseTestHelper._

    def testFlag(p: Parser[_])(utterance :String)(expected: Flag[Any]) = {
      val pRez = parse(p)(utterance)
      pRez must beLike { case Success(e, _) => e === expected }
    }
    
    def aWord = {
      val pRez = parse(someword)("blah ")
      pRez match {
        case x:Success[_] => {
          val pVal = x.get
          println("-------->>>> word = " + pVal)
          pVal match {
            case "blah" => true
            case z      => {println("-------->>>> wrong word = " + z); false}
          }
        }
        case oops:Failure => { println("-------->>>> ooops");
          false
        }
      }
    }

    def checkflagArg = {
      val pRez = parse(flagArg)("a1:a2:a3")
      pRez match {
        case x:Success[_] => {
          val pVal = x.get
          println("---------> args are = " + pVal)
          pVal match {
            case List("a1","a2","a3") => true
            case _      => false
          }
        }
        case oops:Failure => false
      }
    }


    def e1 = testFlag(shortFlgArg)("-p a:b:c")(SimpleArgFlag("p","properties",List("a", "b", "c")))

    def templates = testFlag(shortFlgArg)("-t a:b:c")(SimpleArgFlag("t","templates",List("a", "b", "c")))

    def templateDirs = testFlag(shortFlgArg)("-d a")(SimpleArgFlag("d","templateDirs",List("a")))


    def unknown = {
      val pRez = parse(shortFlg)("-unknown")

      pRez match {
        case x:Success[_] => false
        case oops:Failure => true
      }
    }
    
    def propsWithArgs = {
      val pRez = parse(shortFlgArg)("-p a1:a2:a3")
      pRez match {
        case x: Success[_] => {
          val pVal = x.get

          pVal match {
            case v: SimpleArgFlag[_] if(v.description == "properties") => {
              println("-p a1:a2:a3 went to : " + v.arg)
              println("so first arg is " + v.arg.head)
              if(List("a1:a2:a3") == v.arg) false //should be a list of strings already separated!!
              else if(List("a1","a2","a3") == v.arg) true
              else false
            }
            case _ => false
          }
        }
        case _:Failure => false
      }
    }  

    def lax = testFlag(longFlg)("--lax")(LongFlag("lax", "opposite of Strict, ie does not need a full envpath but will find all thoses paths that share the given prefix"))
  }
}

object parseTestHelper extends CLIParser {
  def parse(p: Parser[Any])(i: String) = {
    parseAll(p, i)
  }
}