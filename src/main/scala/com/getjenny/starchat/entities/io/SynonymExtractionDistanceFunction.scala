package com.getjenny.starchat.entities.io

import scalaz.Scalaz._

object SynonymExtractionDistanceFunction extends Enumeration {
  type SynExtractionDist = Value
  val EMDCOSINE, SUMCOSINE, MEANCOSINE, unknown = Value
  def value(synExtractionDist: String): SynonymExtractionDistanceFunction.Value =
    values.find(_.toString === synExtractionDist).getOrElse(unknown)
}
