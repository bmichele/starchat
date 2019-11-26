package com.getjenny.starchat.entities.io

import scalaz.Scalaz._

object CommonOrSpecificSearch extends Enumeration {
  type CommonOrSpecific = Value
  val COMMON, IDXSPECIFIC, unknown = Value
  def value(mode: String): CommonOrSpecificSearch.Value = values.find(_.toString === mode).getOrElse(unknown)
}
