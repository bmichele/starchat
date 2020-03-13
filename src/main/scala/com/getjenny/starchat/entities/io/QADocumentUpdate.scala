package com.getjenny.starchat.entities.io

import com.getjenny.starchat.entities.persistents.{AggAnnnotations, QADocumentAnnotations, QADocumentCore}

trait QADocumentUpdateBase {
  /* list of document ids to update (bulk editing) */
  val conversation: Option[String]
  /* ID of the conversation (multiple q&a may be inside a conversation) */
  val indexInConversation: Option[Int]
  /* the index of the document in the conversation flow */
  val coreData: Option[QADocumentCore] = None
  /* aggregated annotations */
  val aggAnnotations: Option[AggAnnnotations] = None
  /* core question answer fields */
  val annotations: Option[QADocumentAnnotations] = None
  /* qa and conversation annotations */
  val status: Option[Int] = None
  /* tell whether the document is locked for editing or not, useful for
      a GUI to avoid concurrent modifications, 0 means no operations pending */
  val timestamp: Option[Long] = None /* indexing timestamp, automatically calculated if not provided */
}

case class QADocumentUpdate (
                              id: List[String],
                              override val conversation: Option[String] = None,
                              override val indexInConversation: Option[Int] = None,
                              override val coreData: Option[QADocumentCore] = None,
                              override val aggAnnotations: Option[AggAnnnotations] = None,
                              override val annotations: Option[QADocumentAnnotations] = None,
                              override val status: Option[Int] = None,
                              override val timestamp: Option[Long] = None,
                            ) extends QADocumentUpdateBase

case class QADocumentUpdateByQuery(
                                    override val conversation: Option[String] = None,
                                    override val indexInConversation: Option[Int] = None,
                                    override val coreData: Option[QADocumentCore] = None,
                                    override val aggAnnotations: Option[AggAnnnotations] = None,
                                    override val annotations: Option[QADocumentAnnotations] = None,
                                    override val status: Option[Int] = None,
                                    override val timestamp: Option[Long] = None,
                                  ) extends QADocumentUpdateBase
