package com.getjenny.starchat.analyzer.atoms.http

import scala.util.matching.Regex

object HttpRequestAtomicConstants {

  object ParameterName {
    val Url = "url"
    val HttpMethod = "http-method"
    val InputContentType = "input-content-type"
    val AuthorizationType = "authorization-type"
    val Username = "username"
    val Password = "password"
    val OutputContentType = "output-content-type"
    val OutputStatus = "output-status"
    val OutputData = "output-data"
    val InputQueryTemplate = "input-query-template"

    val System = "system."
    val Temp = "A__temp__."
  }

  object Regex {
    import ParameterName._
    val BaseRegex = s"(?:$Temp)?(?:$System)?http-atom.[a-z0-9]{1,40}."
    val GenericVariableNameRegex: Regex = s"$BaseRegex[a-z0-9_-]{1,40}".r
    val UrlRegex = s"($BaseRegex.$Url)"
    val HttpMethodRegex = s"($BaseRegex.$HttpMethod)"
    val InputContentTypeRegex = s"($BaseRegex.$InputContentType)"
    val InputQueryTemplateRegex = s"($BaseRegex.$InputQueryTemplate)"
    val AuthorizationTypeRegex = s"($BaseRegex.$AuthorizationType)"
    val UsernameRegex = s"($BaseRegex.$Username)"
    val PasswordRegex = s"($BaseRegex.$Password)"
    val OutputContentTypeRegex = s"($BaseRegex.$OutputContentType)"
    val OutputStatusRegex = s"($BaseRegex.$OutputStatus)"
    val OutputDataRegex = s"($BaseRegex.$OutputData)"
  }

}
