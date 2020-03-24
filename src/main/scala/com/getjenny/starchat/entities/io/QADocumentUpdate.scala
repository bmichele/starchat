package com.getjenny.starchat.entities.io

/**
 * Created by Angelo Leto <angelo@getjenny.com> on 24/03/17.
 */

import com.getjenny.starchat.entities.persistents.{AggAnnotations, QADocumentAnnotations, QADocumentCore}

case class QADocumentUpdate (
                              id: List[String],
                              override val conversation: Option[String] = None,
                              override val indexInConversation: Option[Int] = None,
                              override val coreData: Option[QADocumentCore] = None,
                              override val aggAnnotations: Option[AggAnnotations] = None,
                              override val annotations: Option[QADocumentAnnotations] = None,
                              override val status: Option[Int] = None,
                              override val timestamp: Option[Long] = None,
                            ) extends QADocumentUpdateBase


