package com.getjenny.starchat.entities.io

/**
 * Created by Angelo Leto <angelo@getjenny.com> on 17/02/20.
 */

import scalaz.Scalaz._

object StarChatVariables extends Enumeration {
  val GJ_CONVERSATION_ID, /** Conversation ID */
  GJ_LAST_USER_INPUT_TEXT, /** the last user input */
  GJ_CONVERSATION_V1, /** Conversation content: question-answer */
  GJ_CONVERSATION_V2, /** Conversation content: question-answer */

  UNKNOWN = StarChatVariables.Value

  /** normal document, can be returned to the user as response */
  def value(v: String): StarChatVariables.Value = values.find(_.toString === v).getOrElse(UNKNOWN)
}
