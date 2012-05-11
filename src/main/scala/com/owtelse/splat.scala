package com.owtelse

import com.owtelse.parsers._

/**
 * Package object exports stuff needed for normal usage
 */

package object splat {
  import Flags._
  import knownFlags._

  val emptyFlagz = {_:String => emptyFlag }
  def sArgFlag[A] = simpleArgFlag[A] _


  def getShortFlag = getShortFlagz _
  


}

/** bunch of specif flags splat needs **/
object knownFlags {
  import splat._
  
  val props = sArgFlag[String]("p","properties")
  val knownArgFlags = Map(
    "p" -> props ,
    "t" -> emptyFlagz ,
    "x" -> emptyFlagz
  )

  /**note does not return an Option as parser only allows certain flags anyway,
   * so can throw exception if extra flag added to parser but not added to knownArgFlags
   */
  def getShortFlagz(name: String): (String => Flag[Any]) = {
      knownArgFlags(name)
  }
}
