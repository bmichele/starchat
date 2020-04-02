package com.getjenny.starchat.entities.io

/**
 * Created by Angelo Leto <angelo@getjenny.com> on 25/03/20.
 */

import scalaz.Scalaz._

object RefreshPolicy extends Enumeration {
  val `0`, //FIXME remove after components changes
  `1`, //FIXME remove after components changes
  `true`,
  `false`,
  wait_for = RefreshPolicy.Value

  /** normal document, can be returned to the user as response */
  def value(v: String): RefreshPolicy.Value = values.find(_.toString === v).getOrElse(`false`)
}
