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
  def switch: Parser[Any]               = shortFlg | longFlg
  def flagWithArg: Parser[Any]          = shortFlgArg | longFlgArg
  def shortFlgArg: Parser[Flag[_]]      = "-"~>shortArgFlgName~flagArg  ^^ { case flag~argList => shortArgFlag(flag)(argList) }
  def longFlgArg: Parser[Flag[String]]  = "--"~>longFlagName~flagArg    ^^ { case flag~argList => longArgFlag(flag)(argList) }
  def shortFlg: Parser[Flag[String]]    = "-"~>shortFlgName             ^^ { case flag => shortFlag(flag) }
  def longFlg: Parser[Flag[String]]     = "--"~>longFlagName            ^^ { case flag => longFlag(flag) }

  def shortArgFlgName: Parser[String]   = "p" | "t" | "d"
  def shortFlgName: Parser[String]      = "v"
  def longFlagName: Parser[String]      = "lax" | "version"
  def flagArg: Parser[List[String]]     = rep1sep(someword, ":")
  def someword: Parser[String]          = """\w+\b""".r

}

sealed trait dummyconfig 
case class Lax() extends dummyconfig
