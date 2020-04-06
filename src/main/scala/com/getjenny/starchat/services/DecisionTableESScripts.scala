package com.getjenny.starchat.services

import org.elasticsearch.script.Script

trait DecisionTableESScripts {

  private[this] def getScript(script: String): Script = {
    new Script(script)
  }

  private[this] def reindexScriptBody(instance: String): String =
    s"""ctx._source.instance = "${instance}" ; ctx._id = "${instance}" + "|" + ctx._source.state ;"""

  def reindexScript(instance: String): Script = {
    getScript(reindexScriptBody(instance))
  }

}

