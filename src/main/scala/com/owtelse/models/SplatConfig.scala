package com.owtelse.models

import java.io.File
import scalaz.StreamT

/**
 * Created by IntelliJ IDEA.
 * User: robertk
 */

trait SplatConfig {
  val propertyDirs:List[File]
  val templatesDirs:List[File]
}