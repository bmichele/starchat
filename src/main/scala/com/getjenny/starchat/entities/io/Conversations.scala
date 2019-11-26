package com.getjenny.starchat.entities.io

import com.getjenny.starchat.entities.persistents.QADocument

/**
 * Created by Angelo Leto <angelo@getjenny.com> on 18/02/19.
 */

case class Conversation (
                          count: Long = 0,
                          docs: List[QADocument] = List.empty[QADocument]
                        )

case class Conversations (
                           total: Long = 0,
                           conversations: List[Conversation] = List.empty[Conversation]
                         )
