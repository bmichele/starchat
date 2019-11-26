package com.getjenny.starchat.entities.io

import com.getjenny.analyzer.expressions.AnalyzersData

case class AnalyzerEvaluateResponse(build: Boolean, value: Double, data: Option[AnalyzersData],
                                    buildMessage: String)
