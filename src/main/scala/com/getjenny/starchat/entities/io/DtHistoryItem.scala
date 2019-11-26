package com.getjenny.starchat.entities.io

/**
 * Created by Angelo Leto <angelo@getjenny.com> on 15/11/19.
 */

import scalaz.Scalaz._

object DtHistoryType extends Enumeration {
  val REQUESTED,
  CALCULATED,
  DEFAULT = DtHistoryType.Value
  def value(`type`: String): DtHistoryType.Value = values.find(_.toString === `type`).getOrElse(DEFAULT)
}

case class DtHistoryItem(
                          state: String,
                          `type`: DtHistoryType.Value
                        )
