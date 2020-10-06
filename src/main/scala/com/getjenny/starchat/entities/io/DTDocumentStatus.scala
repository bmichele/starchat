package com.getjenny.starchat.entities.io

import scalaz.Scalaz._

object DTDocumentStatus extends Enumeration {
  val DELETED,
  VALID = DTDocumentStatus.Value
  def value(algorithm: String): DTDocumentStatus.Value = values.find(_.toString === algorithm).getOrElse(VALID)
}