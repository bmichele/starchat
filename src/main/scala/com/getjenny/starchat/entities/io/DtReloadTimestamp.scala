package com.getjenny.starchat.entities.io

case class DtReloadTimestamp(
                              indexName: String,
                              timestamp: Long = System.currentTimeMillis(),
                              incremental: Boolean = true
                            )
