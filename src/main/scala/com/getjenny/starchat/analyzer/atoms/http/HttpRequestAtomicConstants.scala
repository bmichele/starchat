package com.getjenny.starchat.analyzer.atoms.http

import scala.util.matching.Regex

object HttpRequestAtomicConstants {

  object ParameterName {
    val url = "url"
    val httpMethod = "http-method"
    val inputContentType = "input-content-type"
    val authorizationType = "authorization-type"
    val token = "token"
    val key = "key"
    val storeTo = "store-to"
    val username = "username"
    val password = "password"
    val outputContentType = "output-content-type"
    val outputStatus = "output-status"
    val outputData = "output-data"
    val outputScore = "output-score"
    val inputQueryTemplate = "input-query-template"
    val inputJson = "input-json"

    val parameterConstantList = List(url, httpMethod, inputContentType, authorizationType,
      token, key, storeTo, username, password, outputContentType, outputStatus, outputData, outputScore,
      inputQueryTemplate, inputJson)

    val temp = "A__temp__."
  }

  object Regex {
    val baseRegex = s"(?:${ParameterName.temp})?http-atom.[a-z0-9]{1,40}."
    val genericVariableNameRegex: Regex = s"$baseRegex[a-z0-9_-]{1,40}".r
    val templateRegex: Regex = "(?<=\\<)(.*?)(?=\\>)".r
  }

  val templateDelimiterPrefix = "<"
  val templateDelimiterSuffix = ">"

}
