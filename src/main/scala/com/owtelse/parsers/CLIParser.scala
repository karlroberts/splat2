package com.owtelse.parsers

import scala.util.parsing.combinator._


/**
 * Parse the command line to configure Splat
 * Created by IntelliJ IDEA.
 * User: robertk
 */
class CLIParser extends JavaTokenParsers {
  import com.owtelse.splat._
  
  def cli: Parser[Any] = rep(opt(flag)) 

  def flag: Parser[Any]                 = switch | flagWithArg
  def switch: Parser[Any]               = shortFlag | longFlag
  def flagWithArg: Parser[Any]          = shortFlagArg | longFlagArg
  def shortFlagArg: Parser[Any]         = shortFlag~flagArg                ^^ { case flag~argList => shortArgFlag(flag)(argList) }
  def longFlagArg: Parser[Any]          = longFlag~flagArg                 ^^ { case flag~argList => println("----->>>> flag = "+ flag +"  arglist = "+ argList); longArgFlag(flag)(argList) }
  def shortFlag: Parser[String]         = "-"~>shortFlagName
  def longFlag: Parser[String]          = "--"~>longFlagName
  
  def shortFlagName: Parser[String]     = "p" | "t" | "d" | "x"
  def longFlagName: Parser[String]      = "lax"
  def flagArg: Parser[List[String]]     = rep1sep(someword, ":")
  def someword: Parser[String]          = """\w+\b""".r

}

sealed trait dummyconfig 
case class Lax() extends dummyconfig
