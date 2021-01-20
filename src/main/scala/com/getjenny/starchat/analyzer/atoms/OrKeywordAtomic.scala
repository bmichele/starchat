package com.getjenny.starchat.analyzer.atoms

/**
 * Created by mal on 23/12/2020.
 */

import com.getjenny.analyzer.atoms.{AbstractAtomic, ExceptionAtomic}
import com.getjenny.analyzer.entities.{AnalyzersDataInternal, Result}
import com.getjenny.starchat.services._
import scalaz.Scalaz._

import scala.util.matching.Regex

/**
 * Takes a list of words or regex and check if any matches the query.
 * orKeyword("foo", "low")  matches "foo and bar" but not "fooish and bar"
 * orKeyword("foo.*", "low") matches "fooish and bar"
 */
class OrKeywordAtomic(val arguments: List[String], restrictedArgs: Map[String, String]) extends AbstractAtomic {
  val atomName: String = "orKeyword"
  if (arguments.isEmpty) throw ExceptionAtomic(atomName + " requires as one or more keywords / regex")
  override def toString: String = "orKeyword(\"" + arguments + "\")"
  val isEvaluateNormalized: Boolean = true

  def evaluate(query: String, data: AnalyzersDataInternal = AnalyzersDataInternal()): Result = {
    arguments.map { word => {
      """\b""" + word + """\b"""
    }.r.findAllIn(query).nonEmpty
    }.contains(true) match {
      case true => Result(score = 1.0)
      case _ => Result(score = 0.0)
    }
  }
}

