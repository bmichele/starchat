package com.getjenny.starchat.entities.io

/**
 * Created by Angelo Leto <angelo@getjenny.com> on 06/04/20.
 */

case class ReindexResult (
                           created: Long,
                           deleted: Long,
                           updated: Long,
                           total: Long,
                           versionConflicts: Long
                         )