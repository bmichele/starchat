package com.getjenny.starchat.entities.io

import com.getjenny.starchat.services.{InstanceRegistryException, InstanceRegistryStatus}
import org.elasticsearch.common.xcontent.XContentBuilder
import org.elasticsearch.common.xcontent.XContentFactory.jsonBuilder

case class InstanceRegistryDocument(timestamp: Option[Long] = None,
                                    enabled: Option[Boolean] = None,
                                    delete: Option[Boolean] = None,
                                    deleted: Option[Boolean] = None,
                                    incremental: Option[Boolean] = None,
                                    staleAge: Option[Long] = None
                                   ) {

  import InstanceRegistryStatus._

  def builder: XContentBuilder = {
    val builder = jsonBuilder().startObject()
    timestamp.foreach(t => builder.field("timestamp", t))
    enabled.foreach(e => builder.field("enabled", e))
    delete.foreach(d => builder.field("delete", d))
    deleted.foreach(d => builder.field("deleted", d))
    incremental.foreach(d => builder.field("incremental", d))
    staleAge.foreach(d => builder.field("staleAge", d))
    builder.endObject()
  }

  def isEmpty: Boolean = {
    deleted.getOrElse(false) || timestamp.isEmpty && enabled.isEmpty && delete.isEmpty
  }

  def status(): InstanceRegistryStatus = {
    (timestamp, enabled, delete, deleted, incremental, staleAge) match {
      case (_, _, _, Some(true), _, _) | (_, None, None, None, _, _) => Missing
      case (_, _, Some(delete), _, _, _) if delete => MarkedForDeletion
      case (_, Some(true), _, _, _, _) => Enabled
      case (_, Some(false), _, _, _, _) => Disabled
      case _ => throw new InstanceRegistryException(s"Instance registry has inconsistent state: " +
        s"(timestamp $timestamp, enabled $enabled, delete $delete, deleted $deleted, incremental $incremental)")
    }
  }
}
