package com.getjenny.starchat.analyzer.atoms.http


import scala.util.matching.Regex

object HttpRequestAtomicConstants {

  object ParameterName {
    val url = "url"
    val httpMethod = "http-method"
    val inputContentType = "input-content-type"
    val authorizationType = "authorization-type"
    val authToken = "auth-token"
    val username = "username"
    val password = "password"
    val outputContentType = "output-content-type"
    val outputStatus = "output-status"
    val outputData = "output-data"
    val inputQueryTemplate = "input-query-template"
    val inputJson = "input-json"

    val system = "system."
    val temp = "A__temp__."
  }

  object Regex {
    import ParameterName._
    val baseRegex = s"(?:$temp)?(?:$system)?http-atom.[a-z0-9]{1,40}."
    val genericVariableNameRegex: Regex = s"$baseRegex[a-z0-9_-]{1,40}".r
  }

  val jsonTemplateDelimiterPrefix = "<"
  val jsonTemplateDelimiterSuffix = ">"
  val queryStringTemplateDelimiterPrefix = "{"
  val queryStringTemplateDelimiterSuffix = "}"

}
