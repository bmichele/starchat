package com.getjenny.starchat.entities.io

import com.getjenny.analyzer.entities.StateVariables

case class AnalyzerEvaluateResponse(build: Boolean, value: Double, data: Option[StateVariables],
                                    buildMessage: String)
