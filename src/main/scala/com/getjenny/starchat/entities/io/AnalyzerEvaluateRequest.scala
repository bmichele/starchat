package com.getjenny.starchat.entities.io

import com.getjenny.analyzer.expressions.AnalyzersData

case class AnalyzerEvaluateRequest(stateName: Option[String] = Some("playground"),
                                   analyzer: String,
                                   query: String,
                                   data: Option[AnalyzersData],
                                   searchAlgorithm: Option[SearchAlgorithm.Value] = Some(SearchAlgorithm.DEFAULT),
                                   evaluationClass: Option[String] = Some("default")
                                  )
