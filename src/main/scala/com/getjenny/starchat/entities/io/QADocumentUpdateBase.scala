package com.getjenny.starchat.entities.io

/**
 * Created by Angelo Leto <angelo@getjenny.com> on 24/03/17.
 */

import com.getjenny.starchat.entities.persistents.{AggAnnotations, QADocumentAnnotations, QADocumentCore}

trait QADocumentUpdateBase {
  /* list of document ids to update (bulk editing) */
  val conversation: Option[String]
  /* ID of the conversation (multiple q&a may be inside a conversation) */
  val indexInConversation: Option[Int]
  /* the index of the document in the conversation flow */
  val coreData: Option[QADocumentCore] = None
  /* aggregated annotations */
  val aggAnnotations: Option[AggAnnotations] = None
  /* core question answer fields */
  val annotations: Option[QADocumentAnnotations] = None
  /* qa and conversation annotations */
  val status: Option[Int] = None
  /* tell whether the document is locked for editing or not, useful for
      a GUI to avoid concurrent modifications, 0 means no operations pending */
  val timestamp: Option[Long] = None /* indexing timestamp, automatically calculated if not provided */
}

