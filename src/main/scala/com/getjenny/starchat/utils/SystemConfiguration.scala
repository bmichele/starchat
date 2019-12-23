package com.getjenny.starchat.utils

import java.util

import akka.event.{Logging, LoggingAdapter}
import com.getjenny.starchat.SCActorSystem
import com.typesafe.config.{Config, ConfigFactory}

import scala.collection.JavaConverters._
import scala.util.{Failure, Success, Try}

object SystemConfiguration {

  private[this] val config: Config = ConfigFactory.load()
  private[this] val log: LoggingAdapter = Logging(SCActorSystem.system, this.getClass)

  def mapFromSystemConfiguration(path: String): Map[String, String] = {
    Try {
      config.getConfig(path)
        .root
        .unwrapped()
        .asScala
        .flatMap {case (k, v) => iterateOverNestedMaps(List.empty, k, v)}
        .foldLeft(Map.empty[String, String])(_ ++ _)
    } match {
      case Success(map) => map
      case Failure(exception) =>
        log.error(exception, "Error while creating map from configuration: ")
        Map.empty
    }
  }

  private[this] def iterateOverNestedMaps(allElements: List[Map[String, String]], key: String, value: Any): List[Map[String, String]] = {
    value match {
      case v: util.Map[String @unchecked, Any @unchecked] => v.asScala.toList
        .flatMap{ case (k, v) => iterateOverNestedMaps(allElements, s"$key.$k", v)}
      case _ => allElements :+ Map((key -> Option(value).getOrElse("").toString))
    }
  }



}
