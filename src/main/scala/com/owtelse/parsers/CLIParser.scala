package com.owtelse.parsers

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.token.StdTokens


/**
 * Parse the command line to configure Splat
 * Created by IntelliJ IDEA.
 * User: robertk
 */
class CLIParser extends JavaTokenParsers {
  import com.owtelse.splat._
  
  def cli: Parser[Any] = rep(opt(flag)) 

  def flag: Parser[Any] = switch | flagWithArg
  def switch: Parser[Any] = shortFlag | longFlag
  def flagWithArg: Parser[Any] = shortFlagArg | longFlag~flagArg
  def shortFlagArg: Parser[Any] = shortFlag~flagArg
  def shortFlag: Parser[Any] = "-"~>shortFlagName                                                                       ^^ { getShortFlag(_) }
  def longFlag: Parser[Any] = "--"~>longFlagName
  
  def shortFlagName: Parser[String] = "p" | "t" | "d" | "x"
  def longFlagName: Parser[Any] = "lax"
  def flagArg: Parser[String] = stringLiteral

}

sealed trait dummyconfig 
case class Lax() extends dummyconfig
