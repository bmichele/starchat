package com.getjenny.starchat.entities.io

import com.getjenny.analyzer.entities.StateVariables

case class AnalyzerEvaluateRequest(stateName: Option[String] = Some("playground"),
                                   analyzer: String,
                                   query: String,
                                   data: Option[StateVariables],
                                   searchAlgorithm: Option[SearchAlgorithm.Value] = Some(SearchAlgorithm.DEFAULT),
                                   evaluationClass: Option[String] = Some("default")
                                  )
