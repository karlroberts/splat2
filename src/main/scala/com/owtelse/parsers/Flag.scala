package com.owtelse.parsers

/**
 * Created by IntelliJ IDEA.
 * User: robertk
 */

sealed trait Flag[+A] {
  val symbol: String
  val description: String
}

case object emptyFlag extends Flag[Nothing] {
  val symbol = "()"
  val description = "empty"
}

case class SimpleFlag[A](symbol: String, description: String) extends Flag[A]

case class SimpleArgFlag[A](symbol: String, description: String, arg: List[String]) extends Flag[A]

case class LongFlag[A](symbol: String, description: String) extends Flag[A]

case class LongArgFlag[A](symbol: String, description: String, arg:  List[String]) extends Flag[A]

case class ComplexFlag[A](symbol: String, long: String, description: String) extends Flag[A]

case class ComplexArgFlag[A](symbol: String, long: String,  description: String, arg:  List[String]) extends Flag[A]

// export curried constructor funcs
object Flags {
  def simpleArgFlag[A](symbol: String, description: String)(args:  List[String]) = SimpleArgFlag[A](symbol, description, args)

}

