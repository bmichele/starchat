package com.getjenny.starchat.entities.io

case class NodeDtLoadingStatus(
                                uuid: Option[String] = Some{""},
                                index: String,
                                timestamp: Option[Long] = Some{0}
                              )
