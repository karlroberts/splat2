package com.owtelse

import com.owtelse.parsers._

/**
 * Package object exports stuff needed for normal usage
 */

package object splat {
  import Flags._
  import knownFlags._

  val emptyFlagz = {_:List[String] => emptyFlag }
  def sArgFlag[A] = simpleArgFlag[A] _
  def lFlag[A] = LongFlag

  // lookup a known flag
  def shortArgFlag = getShortArgFlags _
  def longArgFlag = getLongArgFlags _
  def longFlag = getLongFlags _
  


}

/** bunch of specific pre-canned flags splat supports **/
object knownFlags {
  import splat._

  //short Flags
  val verbose = SimpleFlag[String]("v","verbose")

  //short ArgFlags
  val props = sArgFlag[String]("p","properties")
  val templates = sArgFlag[String]("t","templates")
  val templateDirs = sArgFlag[String]("d","templateDirs")

  //long Flags
  val lax = lFlag("lax", "opposite of Strict, ie does not need a full envpath but will find all thoses paths that share the given prefix")
  val version = lFlag("version", "show version")



  val knownShortArgFlags = Map(
    "p" -> props ,
    "t" -> templates ,
    "d" -> templateDirs ,
    "x" -> emptyFlagz
  )
  val knownShortFlags = Map(
    "v" -> verbose
  )

  val knownLongArgFlags = Map()
  val knownLongFlags = Map(
    "lax" -> lax,
    "version" -> version
  )

  /**note does not return an Option because parser only allows certain flags anyway,
   * so can throw exception if extra flag added to parser but not added to knownArgFlags
   */
  def getShortFlags(name: String): Flag[Any] = {
    knownShortFlags(name)
  }
  def getShortArgFlags(name: String): (List[String] => Flag[Any]) = {
    knownShortArgFlags(name)
  }
  def getLongFlags(name: String): Flag[Any] = {
    knownLongFlags(name)
  }
  def getLongArgFlags(name: String): (List[String] => Flag[Any]) = {
    throw new RuntimeException("No Long Arg Flags implemented! so can't find flag called:- "+ name)
  }
}
