package com.getjenny.starchat.services

import org.elasticsearch.script.Script

trait QuestionAnswerESScripts {

  private[this] def getScript(script: String): Script = {
    new Script(script)
  }

  private[this] val incrementConvIdxCounterScriptBody: String =
    "if (ctx._source.starchatAnnotations == null) {" +
    "  ctx._source.starchatAnnotations = new HashMap() ; " +
    "  ctx._source.starchatAnnotations.convIdxCounter = 0 ; " +
    "}" +
    "ctx._source.starchatAnnotations.convIdxCounter += 1 ; "
  val incrementConvIdxCounterScript = getScript(incrementConvIdxCounterScriptBody)

  private[this] def setIdxCounterScriptBody(value: Int): String =
    "if (ctx._source.starchatAnnotations == null) {" +
    "  ctx._source.starchatAnnotations = new HashMap() ; " +
    "  ctx._source.starchatAnnotations.convIdxCounter = 0 ; " +
    "}" +
    s"ctx._source.starchatAnnotations.convIdxCounter=$value ;"
  def setIdxCounterScript(value: Int): Script = {
    getScript(setIdxCounterScriptBody(value))
  }

}
