package com.owtelse.models

import java.io.File

/**
 * ConfigParts are the result of parsing the Splat command line.
 * They are used to build up a SplatConfig.
 */
sealed trait SplatConfigPart
//placeholder for all the Templates discovered
//case class Templates(files: List[Template]) extends SplatConfigPart

//If given a template Dir keep the details
case class TemplateDir(dir:File) extends SplatConfigPart // todo maybe replace with scalax.io ??

//If given a specific template file keep the details
//case class TemplateFile(template:Template) extends SplatConfigPart // todo maybe replace with scalax.io ??

//
case class Recursive(bool:Boolean) extends SplatConfigPart

case class EnvPath() extends SplatConfigPart

case class Strict() extends SplatConfigPart

case class Multifile() extends SplatConfigPart



/*
import TemplateType._
case class Template(file:File, myType :TemplateType)

object TemplateType extends Enumeration {
  type TemplateType = Value
  val Velocity, Play = Value
}*/
