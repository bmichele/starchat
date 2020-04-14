package com.getjenny.starchat.services

/**
 * Created by Angelo Leto <angelo@getjenny.com> on 10/03/17.
 */

import java.io._

import com.getjenny.starchat.entities.io._
import com.getjenny.starchat.services.esclient.{InstanceRegistryElasticClient, _}
import com.getjenny.starchat.utils.Index
import org.elasticsearch.action.admin.indices.delete.DeleteIndexRequest
import org.elasticsearch.action.admin.indices.open.{OpenIndexRequest, OpenIndexResponse}
import org.elasticsearch.action.admin.indices.settings.put.UpdateSettingsRequest
import org.elasticsearch.action.support.master.AcknowledgedResponse
import org.elasticsearch.client.indices._
import org.elasticsearch.client.{RequestOptions, RestHighLevelClient}
import org.elasticsearch.common.settings._
import org.elasticsearch.common.xcontent.XContentType
import scalaz.Scalaz._

import scala.collection.immutable.List
import scala.io.Source
import scala.util.{Failure, Success, Try}

case class IndexManagementServiceException(message: String = "", cause: Throwable = None.orNull)
  extends Exception(message, cause)

case class LangResourceException(message: String = "", cause: Throwable = None.orNull)
  extends Exception(message, cause)

/**
 * Implements functions, eventually used by IndexManagementResource, for ES index management
 */
object LanguageIndexManagementService {
  private[this] val languageSpecificIndexPrefix = "index_"

  private[this] def analyzerFile(language: String, update: Boolean = false): String = {
    if(update) {
      "/index_management/json_index_spec/" + language + "/update/analyzer.json"
    } else {
      "/index_management/json_index_spec/" + language + "/analyzer.json"
    }
  }

  private[this] val esClients: List[ElasticClient] =
    List(
      ConversationLogsElasticClient,
      KnowledgeBaseElasticClient,
      PriorDataElasticClient,
      DecisionTableElasticClient,
      TermElasticClient
    )

  private[this] def filteredClients(indexSuffixes: Set[String]): List[ElasticClient] = {
    if (indexSuffixes.nonEmpty) {
      esClients.filter { item =>
        indexSuffixes.contains(item.indexSuffix)
      }
    } else {
      esClients
    }
  }

  private[this] object LangResourceType extends Enumeration {
    val STOPWORD,
    STEMMER_OVERRIDE,
    UNSPECIFIED = LangResourceType.Value

    def value(v: String): LangResourceType.Value = values.find(_.toString === v).getOrElse(UNSPECIFIED)
  }

  private[this] val accessoryFilePathTpl = "/index_management/json_index_spec/%1$s/%2$s"
  private[this] val langSpecificDataFiles: List[(String, LangResourceType.Value)] =
    List[(String, LangResourceType.Value)](
      ("stopwords.json", LangResourceType.STOPWORD),
      ("stemmer_override.json", LangResourceType.STEMMER_OVERRIDE)
    )

  private[this] def loadLangSpecificResources(indexName: String,
                                              elasticClient: ElasticClient,
                                              language: String, openCloseIndices: Boolean = false): Unit = {
    val fullIndexName = Index.indexName(indexName, elasticClient.indexSuffix)
    val resourcesJson = langSpecificDataFiles.map { case (file, resType) =>
      val resPath: String = accessoryFilePathTpl.format(language, file)
      val resFileJsonIs: Option[InputStream] = Option {
        getClass.getResourceAsStream(resPath)
      }
      resFileJsonIs match {
        case Some(stream) => (Source.fromInputStream(stream, "utf-8").mkString, resType)
        case _ => ("", resType)
      }
    }.filter { case (json, _) => json =/= "" }
    resourcesJson.foreach { case (resJson, resType) =>
      resType match {
        case LangResourceType.STOPWORD | LangResourceType.STEMMER_OVERRIDE =>
          if (openCloseIndices) openClose(indexName, Set(elasticClient.indexSuffix), "close")
          val updateIndexSettingsReq = new UpdateSettingsRequest().indices(fullIndexName)
            .settings(Settings.builder().loadFromSource(resJson, XContentType.JSON))
          val updateIndexSettingsRes: AcknowledgedResponse = elasticClient.httpClient.indices
            .putSettings(updateIndexSettingsReq, RequestOptions.DEFAULT)
          if (!updateIndexSettingsRes.isAcknowledged) {
            val message = "Failed to apply index settings (" + resType + ") for index: " + indexName
            throw LangResourceException(message)
          }
          if (openCloseIndices) openClose(indexName, Set(elasticClient.indexSuffix), "open")
        case _ =>
          val message = "Bad ResourceType(" + resType + ") for index: " + indexName
          throw LangResourceException(message)
      }
    }
  }

  private[this] def findAllLanguages: List[String] = {
    new File(getClass.getResource("/index_management/json_index_spec/").getFile)
      .listFiles
      .filter(_.isDirectory)
      .map(_.getName)
      .filterNot(x => x === "general" || x === "system")
      .toList
  }

  def create(languages: List[String]): List[IndexManagementResponse] = {
    val languagesToCreate = if(languages.isEmpty) findAllLanguages else languages

    languagesToCreate.map { language =>
      val indexName = s"$languageSpecificIndexPrefix$language"
      val analyzerJsonPath: String = analyzerFile(language = language)
      val analyzerJsonIs: Option[InputStream] = Option {
        getClass.getResourceAsStream(analyzerJsonPath)
      }
      val analyzerJson: String = analyzerJsonIs match {
        case Some(stream) => Source.fromInputStream(stream, "utf-8").mkString
        case _ =>
          val message = "Check the file: (" + analyzerJsonPath + ")"
          throw new FileNotFoundException(message)
      }

      val operationsMessage: List[(String, Boolean)] = esClients.map { item =>
        val client: RestHighLevelClient = item.httpClient

        val jsonInStream: Option[InputStream] = Option {
          getClass.getResourceAsStream(item.mappingPath)
        }

        val schemaJson: String = jsonInStream match {
          case Some(stream) => Source.fromInputStream(stream, "utf-8").mkString
          case _ =>
            val message = "Check the file: (" + item.mappingPath + ")"
            throw new FileNotFoundException(message)
        }

        val fullIndexName = s"$languageSpecificIndexPrefix$language.${item.indexSuffix}"

        val createIndexReq = new CreateIndexRequest(fullIndexName).settings(
          Settings.builder().loadFromSource(analyzerJson, XContentType.JSON)
            .put("index.number_of_shards", item.numberOfShards)
            .put("index.number_of_replicas", item.numberOfReplicas)
        ).source(schemaJson, XContentType.JSON)

        val createIndexRes: CreateIndexResponse = client.indices.create(createIndexReq, RequestOptions.DEFAULT)

        loadLangSpecificResources(indexName = indexName,
          elasticClient = item,
          language = language, openCloseIndices = true)

        (s"${item.indexSuffix}($fullIndexName, ${createIndexRes.isAcknowledged})",
          createIndexRes.isAcknowledged)
      }

      val message = "IndexCreation: " + operationsMessage.map { case (msg, _) => msg }.mkString(" ")
      val check = operationsMessage.nonEmpty && operationsMessage.forall { case (_, ck) => ck }

      IndexManagementResponse(message = message, check = check)
    }
  }

  def remove(indexName: String,
             indexSuffix: Set[String] = Set.empty[String]): IndexManagementResponse = {

    val operationsMessage: List[(String, Boolean)] = filteredClients(indexSuffix).map(item => {
      if (!item.enableDeleteIndex) {
        val message: String = "operation is not allowed, contact system administrator"
        throw IndexManagementServiceException(message)
      }

      val client: RestHighLevelClient = item.httpClient

      val fullIndexName = Index.indexName(indexName, item.indexSuffix)

      val deleteIndexReq = new DeleteIndexRequest(fullIndexName)

      val deleteIndexRes: AcknowledgedResponse = client.indices.delete(deleteIndexReq, RequestOptions.DEFAULT)

      (item.indexSuffix + "(" + fullIndexName + ", " + deleteIndexRes.isAcknowledged + ")",
        deleteIndexRes.isAcknowledged)
    })

    val message = "IndexDeletion: " + operationsMessage.map { case (msg, _) => msg }.mkString(" ")
    val check = operationsMessage.nonEmpty && operationsMessage.forall { case (_, ck) => ck }
    IndexManagementResponse(message = message, check = check)
  }

  def check(indexName: String,
            indexSuffix: Set[String] = Set.empty[String]): IndexManagementResponse = {

    val operations: List[(String, Boolean)] = filteredClients(indexSuffix).map { item =>
      val client: RestHighLevelClient = item.httpClient
      val fullIndexName = Index.indexName(indexName, item.indexSuffix)

      val getMappingsReq: GetMappingsRequest = new GetMappingsRequest()
        .indices(fullIndexName)

      Try(client.indices.getMapping(getMappingsReq, RequestOptions.DEFAULT)) match {
        case Success(mappingsRes) =>
          val check = mappingsRes.mappings.containsKey(fullIndexName)
          (item.indexSuffix + "(" + fullIndexName + ", " + check + ")", check)
        case Failure(exception) =>
          (item.indexSuffix + "(" + fullIndexName + ", false)", false)
      }
    }

    val (messages, _) = operations.unzip
    val check = operations.nonEmpty && operations.forall { case (_, ck) => ck }
    IndexManagementResponse(message = "IndexCheck: " + messages.mkString(" "),
      check = check)
  }

  def openClose(indexName: String, indexSuffix: Set[String] = Set.empty[String],
                operation: String): List[OpenCloseIndex] = {
    filteredClients(indexSuffix).map(item => {
      val client: RestHighLevelClient = item.httpClient
      val fullIndexName = Index.indexName(indexName, item.indexSuffix)
      operation match {
        case "close" =>
          val closeIndexReq = new CloseIndexRequest(fullIndexName)
          val closeIndexRes: AcknowledgedResponse = client.indices.close(closeIndexReq, RequestOptions.DEFAULT)
          OpenCloseIndex(indexName = indexName, indexSuffix = item.indexSuffix, fullIndexName = fullIndexName,
            operation = operation, acknowledgement = closeIndexRes.isAcknowledged)
        case "open" =>
          val openIndexReq = new OpenIndexRequest().indices(fullIndexName)
          val openIndexRes: OpenIndexResponse = client.indices.open(openIndexReq, RequestOptions.DEFAULT)
          OpenCloseIndex(indexName = indexName, indexSuffix = item.indexSuffix, fullIndexName = fullIndexName,
            operation = operation, acknowledgement = openIndexRes.isAcknowledged)
        case _ => throw IndexManagementServiceException("operation not supported on index: " + operation)
      }
    })
  }

  def updateSettings(indexName: String,
                     indexSuffix: Set[String] = Set.empty[String]): IndexManagementResponse = {
    val language = Index.languageFromSpecificLanguageIndexName(indexName)

    val analyzerJsonPath: String = analyzerFile(language = language, update = true)
    val analyzerJsonIs: Option[InputStream] = Option {
      getClass.getResourceAsStream(analyzerJsonPath)
    }
    val analyzerJson: String = analyzerJsonIs match {
      case Some(stream) => Source.fromInputStream(stream, "utf-8").mkString
      case _ =>
        val message = "file not found: (" + analyzerJsonPath + ")"
        throw new FileNotFoundException(message)
    }

    val operationsMessage: List[(String, Boolean)] = filteredClients(indexSuffix).map(item => {
      val client: RestHighLevelClient = item.httpClient

      val fullIndexName = Index.indexName(indexName, item.indexSuffix)

      val updateIndexSettingsReq = new UpdateSettingsRequest().indices(fullIndexName)
        .settings(Settings.builder().loadFromSource(analyzerJson, XContentType.JSON))

      val updateIndexSettingsRes: AcknowledgedResponse = client.indices
        .putSettings(updateIndexSettingsReq, RequestOptions.DEFAULT)

      loadLangSpecificResources(indexName = indexName,
        elasticClient = item,
        language = language)

      (item.indexSuffix + "(" + fullIndexName + ", " + updateIndexSettingsRes.isAcknowledged + ")",
        updateIndexSettingsRes.isAcknowledged)
    })

    val message = "IndexSettingsUpdate: " + operationsMessage.map { case (msg, _) => msg }.mkString(" ")
    val check = operationsMessage.nonEmpty && operationsMessage.forall { case (_, ck) => ck }
    IndexManagementResponse(message = message, check = check)
  }

  def updateMappings(indexName: String,
                     indexSuffix: Set[String] = Set.empty[String]): IndexManagementResponse = {
    val operationsMessage: List[(String, Boolean)] = filteredClients(indexSuffix).map(item => {
      val client: RestHighLevelClient = item.httpClient

      val fullIndexName = Index.indexName(indexName, item.indexSuffix)

      val jsonInStream: Option[InputStream] = Option {
        getClass.getResourceAsStream(item.updateMappingPath)
      }

      val schemaJson: String = jsonInStream match {
        case Some(stream) => Source.fromInputStream(stream, "utf-8").mkString
        case _ =>
          val message = "Check the file: (" + item.updateMappingPath + ")"
          throw new FileNotFoundException(message)
      }

      val putMappingReq = new PutMappingRequest(fullIndexName)
        .source(schemaJson, XContentType.JSON)

      val putMappingRes: AcknowledgedResponse = client.indices
        .putMapping(putMappingReq, RequestOptions.DEFAULT)

      (item.indexSuffix + "(" + indexName + ", " + putMappingRes.isAcknowledged + ")",
        putMappingRes.isAcknowledged)
    })

    val message = "IndexUpdateMappings: " + operationsMessage.mkString(" ")
    val check = operationsMessage.nonEmpty && operationsMessage.forall { case (_, ck) => ck }
    IndexManagementResponse(message = message, check = check)
  }

  def refresh(indexName: String,
              indexSuffix: Set[String] = Set.empty[String]): RefreshIndexResults = {
    val operationsResults: List[RefreshIndexResult] = filteredClients(indexSuffix).map(item => {
      val fullIndexName = Index.indexName(indexName, item.indexSuffix)
      val refreshIndexRes: RefreshIndexResult = item.refresh(fullIndexName)
      if (refreshIndexRes.failedShardsN > 0) {
        val indexRefreshMessage = item.indexSuffix + "(" + fullIndexName + ", " + refreshIndexRes.failedShardsN + ")"
        throw new Exception(indexRefreshMessage)
      }

      refreshIndexRes
    })

    RefreshIndexResults(results = operationsResults)
  }

  def indices: List[String] = {
    val request = new GetIndexRequest(s"$languageSpecificIndexPrefix*")
    val res = InstanceRegistryElasticClient.httpClient.indices()
      .get(request, RequestOptions.DEFAULT)
    res.getIndices.toList
  }
}


