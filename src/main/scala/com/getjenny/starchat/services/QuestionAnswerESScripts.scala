package com.getjenny.starchat.services

import org.elasticsearch.script.Script

trait QuestionAnswerESScripts {

  private[this] def getScript(script: String): Script = {
    new Script(script)
  }

  private[this] val initializeAnnotationsBody = "if (ctx._source.starchatAnnotations == null) {" +
    "  ctx._source.starchatAnnotations = new HashMap() ; " +
    "  ctx._source.starchatAnnotations.convIdxCounter = 0 ; " +
    "  ctx._source.starchatAnnotations.convIdxQuestionsCounter = 0 ; " +
  "}"

  private[this] val incrementConvIdxCounterScriptBody: String = {
    initializeAnnotationsBody +
    "ctx._source.starchatAnnotations.convIdxCounter += 1 ; "
  }

  private[this] val incrementConvIdxQuestionsCounterScriptBody: String = {
    initializeAnnotationsBody +
      "ctx._source.starchatAnnotations.convIdxQuestionsCounter += 1 ; "
  }

  private[this] val resetQuestionFieldScriptBody: String = {
    "ctx._source.question = '' ; "
  }

  val incrementConvIdxCounterScript: Script = getScript(incrementConvIdxCounterScriptBody)
  val incrementConvIdxQuestionsCounterScript: Script = getScript(incrementConvIdxQuestionsCounterScriptBody)
  val resetQuestionFieldScript: Script = getScript(resetQuestionFieldScriptBody)

  private[this] def setIdxCounterScriptBody(value: Int): String = {
      initializeAnnotationsBody +
      s"ctx._source.starchatAnnotations.convIdxCounter=$value ;"
  }

  private[this] def setIdxQuestionCounterScriptBody(value: Int): String = {
    initializeAnnotationsBody +
      s"ctx._source.starchatAnnotations.convIdxQuestionsCounter=$value ;"
  }

  def setIdxCounterScript(value: Int): Script = {
    getScript(setIdxCounterScriptBody(value))
  }

  def setIdxQuestionCounterScript(value: Int): Script = {
    getScript(setIdxQuestionCounterScriptBody(value))
  }
}
