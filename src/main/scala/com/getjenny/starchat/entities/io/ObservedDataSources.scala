package com.getjenny.starchat.entities.io

import scalaz.Scalaz._

object ObservedDataSources extends Enumeration {
  type ObservedDataSource = Value
  val KNOWLEDGEBASE, CONV_LOGS, unknown = Value
  def value(enumValue: String) = values.find(_.toString === enumValue).getOrElse(unknown)
}
