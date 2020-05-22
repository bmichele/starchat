package com.getjenny.starchat.analyzer.atoms

/**
  * Created by angelo on 11/04/17.
  */

import com.getjenny.analyzer.atoms.{AbstractAtomic, ExceptionAtomic}
import com.getjenny.analyzer.expressions.{AnalyzersDataInternal, Result}
import com.getjenny.analyzer.util.VectorUtils._
import com.getjenny.starchat.analyzer.utils.TextToVectorsTools
import com.getjenny.starchat.entities.io.CommonOrSpecificSearch
import com.getjenny.starchat.entities.persistents.TextTerms
import com.getjenny.starchat.services._
import com.getjenny.starchat.utils.Index
import com.getjenny.analyzer.expressions.Context

import scala.collection.immutable

class W2VCosineStateAtomic(val arguments: List[String], restrictedArgs: Map[String, String]) extends AbstractAtomic {
  /**
    * cosine distance between sentences renormalized at [0, 1]: (cosine + 1)/2
    *
    * state_lost_password_cosine = Cosine("lost password")
    * state_lost_password_cosine.evaluate("I'm desperate, I've lost my password")
    *
    */

  val state: String = arguments.headOption match {
    case Some(t) => t
    case _ =>
      throw ExceptionAtomic("similarState requires an argument")
  }

  val commonOrSpecific: CommonOrSpecificSearch.Value = arguments.lastOption match {
    case Some(t) => CommonOrSpecificSearch.value(t)
    case _ => CommonOrSpecificSearch.COMMON
  }

  override def toString: String = "similarState(\"" + state + "\")"

  val analyzerService: AnalyzerService.type = AnalyzerService

  val isEvaluateNormalized: Boolean = true

  def evaluate(query: String, data: AnalyzersDataInternal = AnalyzersDataInternal()): Result = {
    val indexName = Index.resolveIndexName(restrictedArgs(data.context.indexName), commonOrSpecific)

    val queriesTerms = AnalyzerService.analyzersMap(indexName).analyzerMap.get(state) match {
      case Some(q) =>
        analyzerService.log.info(s"$toString : initialized")
        q.queriesTerms
      case None =>
        analyzerService.log.error(s"$toString : state does not exists")
        List.empty
    }

    val distance = queriesTerms
      .map(item => TextToVectorsTools.sumOfTermsVectors(item))
      .map { case (sentenceVector, reliabilityFactor) =>
        val (querySentenceVector, queryReliabilityFactor) = TextToVectorsTools.sumOfVectorsFromText(indexName, query)
        (1.0 - cosineDist(sentenceVector, querySentenceVector)) * (reliabilityFactor * queryReliabilityFactor)
      }

    val dist = if (distance.nonEmpty) distance.max else 0.0
    Result(score = dist)
  }

  // Similarity is normally the cosine itself. The threshold should be at least
  // angle < pi/2 (cosine > 0), but for synonyms let's put cosine > 0.6, i.e. self.evaluate > 0.8
  override val matchThreshold: Double = 0.8
}
