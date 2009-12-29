package com.owtelse.parsers

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.token.StdTokens


/**
 * Parse the command line to configure Splat
 * Created by IntelliJ IDEA.
 * User: robertk
 */
class CLIParser extends JavaTokenParsers {
  
  def cli: Parser[Any] = rep(opt(flag)) 

  def flag: Parser[Any] = switch | flagWithArg
  def switch: Parser[Any] = shortFlag | longFlag
  def flagWithArg: Parser[Any] = shortFlag~flagArg | longFlag~flagArg
  def shortFlag: Parser[Any] = "-"~shortFlagName
  def longFlag: Parser[Any] = "--"~longFlagName
  
  def shortFlagName: Parser[Any] = "p" | "t" | "d"
  def longFlagName: Parser[Any] = "lax" ^^ {_ => Lax()}
  def flagArg: Parser[String] = stringLiteral

}

sealed trait dummyconfig 
case class Lax() extends dummyconfig
