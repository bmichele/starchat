package com.getjenny.starchat.entities.io

/**
 * Created by Angelo Leto <angelo@getjenny.com> on 06/03/20.
 */

case class UpdateByQueryResult (
                                 timedOut: Boolean,
                                 totalDocs: Long,
                                 updatedDocs: Long,
                                 versionConflicts: Long
                               )
