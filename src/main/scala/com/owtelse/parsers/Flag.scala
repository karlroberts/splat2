package com.owtelse.parsers

/**
 * Created by IntelliJ IDEA.
 * User: robertk
 */

sealed trait Flag[+A] {
  val description: String
}

case object emptyFlag extends Flag[Nothing] {
  val description = "empty"
}

case class SimpleFlag[A](short: String, description: String) extends Flag[A]

case class SimpleArgFlag[A](short: String, description: String, arg: String) extends Flag[A]

case class LongFlag[A](long: String, description: String) extends Flag[A]

case class LongArgFlag[A](long: String, description: String, arg: String) extends Flag[A]

case class ComplexFlag[A](short: String, long: String, description: String) extends Flag[A]

case class ComplexArgFlag[A](short: String, long: String,  description: String, arg: String) extends Flag[A]

// export curried constructor funcs
object Flags {
  def simpleArgFlag[A](short: String, description: String)(arg: String) = SimpleArgFlag[A](short, description, arg)
}

