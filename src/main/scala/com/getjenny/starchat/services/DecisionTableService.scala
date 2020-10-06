package com.getjenny.starchat.services

/**
 * Created by Angelo Leto <angelo@getjenny.com> on 01/07/16.
 */

import java.io.File
import java.util

import akka.event.{Logging, LoggingAdapter}
import com.getjenny.starchat.SCActorSystem
import com.getjenny.starchat.analyzer.utils.TokenToVector
import com.getjenny.starchat.entities.io._
import com.getjenny.starchat.entities.persistents._
import com.getjenny.starchat.services.esclient.crud.IndexLanguageCrud
import com.getjenny.starchat.services.esclient.{CloneDtElasticClient, DecisionTableElasticClient}
import com.getjenny.starchat.utils.Index
import org.apache.lucene.search.join._
import org.elasticsearch.action.search.SearchType
import org.elasticsearch.client.RequestOptions
import org.elasticsearch.index.query.{BoolQueryBuilder, InnerHitBuilder, QueryBuilder, QueryBuilders}
import org.elasticsearch.index.reindex.{BulkByScrollResponse, DeleteByQueryRequest, ReindexRequest}
import org.elasticsearch.script.Script
import org.elasticsearch.search.SearchHits
import org.elasticsearch.search.aggregations.AggregationBuilders
import scalaz.Scalaz._

import scala.collection.JavaConverters._
import scala.collection.immutable.{List, Map}

case class DecisionTableServiceException(message: String = "", cause: Throwable = None.orNull)
  extends Exception(message, cause)

/**
 * Implements functions, eventually used by DecisionTableResource, for searching, get next response etc
 */
object DecisionTableService extends AbstractDataService with DecisionTableESScripts {
  override val elasticClient: DecisionTableElasticClient.type = DecisionTableElasticClient
  private[this] val termService: TermService.type = TermService
  private[this] val log: LoggingAdapter = Logging(SCActorSystem.system, this.getClass.getCanonicalName)

  private[this] val queriesScoreMode: Map[String, ScoreMode] =
    Map[String, ScoreMode]("min" -> ScoreMode.Min,
      "max" -> ScoreMode.Max, "avg" -> ScoreMode.Avg, "total" -> ScoreMode.Total)

  def handleNgrams(indexName: String, analyzer: String, queries: String,
                   dtDocuments: List[SearchDTDocumentsAndNgrams]): List[SearchDTDocument] = {
    val tokenizerRequest = TokenizerQueryRequest(tokenizer = analyzer, text = queries)
    val tokenizerResponse = termService.esTokenizer(indexName, tokenizerRequest)
    val queryTokens = tokenizerResponse.tokens.map(_.token)

    val ngramsIndex = (dtDocuments.flatMap { case SearchDTDocumentsAndNgrams(_, ngrams) => ngrams }
      .flatten ++ queryTokens).distinct.zipWithIndex.toMap

    val queryVector = TokenToVector.tokensToVector(queryTokens, ngramsIndex)

    dtDocuments.map { case SearchDTDocumentsAndNgrams(searchDocument, queriesNgrams) =>
      val score = queriesNgrams.map(ngrams => {
        1.0 - TokenToVector.cosineDist(queryVector, TokenToVector.tokensToVector(ngrams, ngramsIndex))
      }).max
      (searchDocument, score)
    }.map { case (searchDtDocument, score) =>
      val document: DTDocument = searchDtDocument.document
      SearchDTDocument(score = score.toFloat, document = document)
    }
  }

  private[this] def documentSearchQueries(indexName: String,
                                          value: String,
                                          minScore: Float,
                                          boostExactMatchFactor: Float,
                                          documentSearch: DTDocumentSearch
                                         ): (QueryBuilder, Option[(String, SearchAlgorithm.Value)]) = {
    val searchAlgorithm = documentSearch.searchAlgorithm.getOrElse(SearchAlgorithm.DEFAULT)
    searchAlgorithm match {
      case SearchAlgorithm.AUTO | SearchAlgorithm.DEFAULT =>
        val (scriptBody, matchQueryEs, analyzer, algorithm) = if (documentSearch.queries.getOrElse("").length > 3) {
          (
            "return doc['queries.query.ngram_3'] ;",
            "queries.query.ngram_3",
            "ngram3",
            SearchAlgorithm.NGRAM3

          )
        } else {
          (
            "return doc['queries.query.ngram_2'] ;",
            "queries.query.ngram_2",
            "ngram2",
            SearchAlgorithm.NGRAM2
          )
        }
        val script: Script = new Script(scriptBody)
        (QueryBuilders.nestedQuery(
          "queries",
          QueryBuilders.boolQuery()
            .must(QueryBuilders.matchQuery(matchQueryEs, value)),
          queriesScoreMode.getOrElse(elasticClient.queriesScoreMode, ScoreMode.Max)
        ).ignoreUnmapped(true)
          .innerHit(new InnerHitBuilder().addScriptField("terms", script).setSize(100)),
          Option(analyzer -> algorithm))
      case SearchAlgorithm.STEM_BOOST_EXACT =>
        (
          QueryBuilders.nestedQuery(
            "queries",
            QueryBuilders.boolQuery()
              .must(QueryBuilders.matchQuery("queries.query.stem", value))
              .should(QueryBuilders.matchPhraseQuery("queries.query.raw", value)
                .boost(1 + (minScore * boostExactMatchFactor))
              ),
            queriesScoreMode.getOrElse(elasticClient.queriesScoreMode, ScoreMode.Max)
          ).ignoreUnmapped(true).innerHit(new InnerHitBuilder().setSize(100)),
          None)
      case SearchAlgorithm.SHINGLES2 =>
        (QueryBuilders.nestedQuery(
          "queries",
          QueryBuilders.boolQuery()
            .must(QueryBuilders.matchQuery("queries.query.shingles_2", value)),
          queriesScoreMode.getOrElse(elasticClient.queriesScoreMode, ScoreMode.Max)
        ).ignoreUnmapped(true).innerHit(new InnerHitBuilder().setSize(100)),
          None)
      case SearchAlgorithm.SHINGLES3 =>
        (QueryBuilders.nestedQuery(
          "queries",
          QueryBuilders.boolQuery()
            .must(QueryBuilders.matchQuery("queries.query.shingles_3", value)),
          queriesScoreMode.getOrElse(elasticClient.queriesScoreMode, ScoreMode.Max)
        ).ignoreUnmapped(true).innerHit(new InnerHitBuilder().setSize(100)),
          None)
      case SearchAlgorithm.SHINGLES4 =>
        (QueryBuilders.nestedQuery(
          "queries",
          QueryBuilders.boolQuery()
            .must(QueryBuilders.matchQuery("queries.query.shingles_4", value)),
          queriesScoreMode.getOrElse(elasticClient.queriesScoreMode, ScoreMode.Max)
        ).ignoreUnmapped(true).innerHit(new InnerHitBuilder().setSize(100)),
          None)
      case SearchAlgorithm.STEM_SHINGLES2 =>
        (QueryBuilders.nestedQuery(
          "queries",
          QueryBuilders.boolQuery()
            .must(QueryBuilders.matchQuery("queries.query.stemmed_shingles_2", value)),
          queriesScoreMode.getOrElse(elasticClient.queriesScoreMode, ScoreMode.Max)
        ).ignoreUnmapped(true).innerHit(new InnerHitBuilder().setSize(100)),
          None)
      case SearchAlgorithm.STEM_SHINGLES3 =>
        (QueryBuilders.nestedQuery(
          "queries",
          QueryBuilders.boolQuery()
            .must(QueryBuilders.matchQuery("queries.query.stemmed_shingles_3", value)),
          queriesScoreMode.getOrElse(elasticClient.queriesScoreMode, ScoreMode.Max)
        ).ignoreUnmapped(true).innerHit(new InnerHitBuilder().setSize(100)),
          None)
      case SearchAlgorithm.STEM_SHINGLES4 =>
        (QueryBuilders.nestedQuery(
          "queries",
          QueryBuilders.boolQuery()
            .must(QueryBuilders.matchQuery("queries.query.stemmed_shingles_4", value)),
          queriesScoreMode.getOrElse(elasticClient.queriesScoreMode, ScoreMode.Max)
        ).ignoreUnmapped(true).innerHit(new InnerHitBuilder().setSize(100)),
          None)
      case SearchAlgorithm.NGRAM2 =>
        val scriptBody = "return doc['queries.query.ngram_2'] ;"
        val script: Script = new Script(scriptBody)
        (QueryBuilders.nestedQuery(
          "queries",
          QueryBuilders.boolQuery()
            .must(QueryBuilders.matchQuery("queries.query.ngram_2", value)),
          queriesScoreMode.getOrElse(elasticClient.queriesScoreMode, ScoreMode.Max)
        ).ignoreUnmapped(true)
          .innerHit(new InnerHitBuilder().addScriptField("terms", script).setSize(100)),
          Option("ngram2" -> searchAlgorithm))
      case SearchAlgorithm.STEM_NGRAM2 =>
        val scriptBody = "return doc['queries.query.stemmed_ngram_2'] ;"
        val script: Script = new Script(scriptBody)
        (QueryBuilders.nestedQuery(
          "queries",
          QueryBuilders.boolQuery()
            .must(QueryBuilders.matchQuery("queries.query.stemmed_ngram_2", value)),
          queriesScoreMode.getOrElse(elasticClient.queriesScoreMode, ScoreMode.Max)
        ).ignoreUnmapped(true)
          .innerHit(new InnerHitBuilder().addScriptField("terms", script).setSize(100)),
          Option("stemmed_ngram2" -> searchAlgorithm))
      case SearchAlgorithm.NGRAM3 =>
        val scriptBody = "return doc['queries.query.ngram_3'] ;"
        val script: Script = new Script(scriptBody)
        (QueryBuilders.nestedQuery(
          "queries",
          QueryBuilders.boolQuery()
            .must(QueryBuilders.matchQuery("queries.query.ngram_3", value)),
          queriesScoreMode.getOrElse(elasticClient.queriesScoreMode, ScoreMode.Max)
        ).ignoreUnmapped(true)
          .innerHit(new InnerHitBuilder().addScriptField("terms", script).setSize(100)),
          Option("ngram3" -> searchAlgorithm))
      case SearchAlgorithm.STEM_NGRAM3 =>
        val scriptBody = "return doc['queries.query.stemmed_ngram_3'] ;"
        val script: Script = new Script(scriptBody)
        (QueryBuilders.nestedQuery(
          "queries",
          QueryBuilders.boolQuery()
            .must(QueryBuilders.matchQuery("queries.query.stemmed_ngram_3", value)),
          queriesScoreMode.getOrElse(elasticClient.queriesScoreMode, ScoreMode.Max)
        ).ignoreUnmapped(true)
          .innerHit(new InnerHitBuilder().addScriptField("terms", script).setSize(100)),
          Option("stemmed_ngram3", searchAlgorithm))
      case SearchAlgorithm.NGRAM4 =>
        val scriptBody = "return doc['queries.query.ngram_4'] ;"
        val script: Script = new Script(scriptBody)
        (QueryBuilders.nestedQuery(
          "queries",
          QueryBuilders.boolQuery()
            .must(QueryBuilders.matchQuery("queries.query.ngram_4", value)),
          queriesScoreMode.getOrElse(elasticClient.queriesScoreMode, ScoreMode.Max)
        ).ignoreUnmapped(true)
          .innerHit(new InnerHitBuilder().addScriptField("terms", script).setSize(100)),
          Option("ngram4" -> searchAlgorithm))
      case SearchAlgorithm.STEM_NGRAM4 =>
        val scriptBody = "return doc['queries.query.stemmed_ngram_4'] ;"
        val script: Script = new Script(scriptBody)
        (QueryBuilders.nestedQuery(
          "queries",
          QueryBuilders.boolQuery()
            .must(QueryBuilders.matchQuery("queries.query.stemmed_ngram_4", value)),
          queriesScoreMode.getOrElse(elasticClient.queriesScoreMode, ScoreMode.Max)
        ).ignoreUnmapped(true)
          .innerHit(new InnerHitBuilder().addScriptField("terms", script).setSize(100)),
          Option("stemmed_ngram4" -> searchAlgorithm))
      case _ => throw DecisionTableServiceException("Unknown SearchAlgorithm: " + searchAlgorithm)
    }

  }

  def search(indexName: String, documentSearch: DTDocumentSearch): SearchDTDocumentsResults = {

    val minScore = documentSearch.minScore.getOrElse(
      Option {
        elasticClient.queryMinThreshold
      }.getOrElse(0.0f)
    )

    val boostExactMatchFactor = documentSearch.boostExactMatchFactor.getOrElse(
      Option {
        elasticClient.boostExactMatchFactor
      }.getOrElse(1.0f)
    )

    val boolQueryBuilder: BoolQueryBuilder = QueryBuilders.boolQuery()

    documentSearch.state match {
      case Some(value) => boolQueryBuilder.must(QueryBuilders.termQuery("state", value))
      case _ => ;
    }

    documentSearch.status match {
      case Some(value) =>
        boolQueryBuilder.must(QueryBuilders.termQuery("status", value.toString))
      case _ => ;
    }

    documentSearch.timestampGte match {
      case Some(value) =>
        boolQueryBuilder.filter(QueryBuilders.rangeQuery("timestamp").gte(value))
      case _ => ;
    }

    documentSearch.timestampLte match {
      case Some(value) =>
        boolQueryBuilder.filter(QueryBuilders.rangeQuery("timestamp").lte(value))
      case _ => ;
    }

    documentSearch.executionOrder match {
      case Some(value) =>
        boolQueryBuilder.must(QueryBuilders.matchQuery("execution_order", value))
      case _ => ;
    }

    val (queryBuilder, isNgram) = documentSearch.queries.map(x => {
      val (q, ngramsAlgorithm) = documentSearchQueries(indexName, x, minScore, boostExactMatchFactor, documentSearch)
      boolQueryBuilder.must(q) -> ngramsAlgorithm
    }).getOrElse(boolQueryBuilder -> None)

    val queryExtractorFunction = isNgram.map { case (_, alg) =>
      val sliding = alg match {
        case SearchAlgorithm.STEM_NGRAM2 | SearchAlgorithm.NGRAM2 => 2
        case SearchAlgorithm.STEM_NGRAM3 | SearchAlgorithm.NGRAM3 => 3
        case SearchAlgorithm.STEM_NGRAM4 | SearchAlgorithm.NGRAM4 => 4
        case _ => 2
      }
      queryAndNgramExtractor(sliding)
    }.getOrElse(orderedQueryExtractor)

    val indexLanguageCrud = IndexLanguageCrud(elasticClient, indexName)
    val documents = indexLanguageCrud.read(queryBuilder, version = Option(true),
      from = documentSearch.from.orElse(Option(0)),
      maxItems = documentSearch.size.orElse(Option(10)),
      entityManager = new SearchDTDocumentEntityManager(queryExtractorFunction))

    val res = isNgram.map { case (analyzer, _) => handleNgrams(indexName, analyzer,
      documentSearch.queries.getOrElse(""), documents)
    }
      .getOrElse(documents.map(_.documents))

    val maxScore: Float = if (res.nonEmpty) {
      res.maxBy(_.score).score
    } else {
      0.0f
    }

    val total: Int = res.length
    SearchDTDocumentsResults(total = total, maxScore = maxScore, hits = res)
  }

  def searchDtQueries(indexName: String,
                      analyzerEvaluateRequest: AnalyzerEvaluateRequest): SearchDTDocumentsResults = {
    val dtDocumentSearch: DTDocumentSearch =
      DTDocumentSearch(
        from = Option {
          0
        },
        size = Option {
          10000
        },
        minScore = Option {
          elasticClient.queryMinThreshold
        },
        executionOrder = None: Option[Int],
        boostExactMatchFactor = Option {
          elasticClient.boostExactMatchFactor
        },
        state = None: Option[String],
        queries = Option {
          analyzerEvaluateRequest.query
        },
        searchAlgorithm = analyzerEvaluateRequest.searchAlgorithm,
        evaluationClass = analyzerEvaluateRequest.evaluationClass,
        status = Some(DTDocumentStatus.VALID)
      )

    this.search(indexName, dtDocumentSearch)
  }

  def resultsToMap(results: SearchDTDocumentsResults): Map[String, Any] = {
    Map(
      "dt_queries_search_result" -> results.hits.map {
        doc => (doc.document.state, (doc.score, doc))
      }.toMap
    )
  }

  def create(indexName: String, document: DTDocument, check: Boolean = true,
             refreshPolicy: RefreshPolicy.Value): IndexDocumentResult = {
    val indexLanguageCrud = IndexLanguageCrud(elasticClient, indexName)

    if (check) {
      //TODO: to be implemented checks on state, analyzers, actions ...
    }

    val response = indexLanguageCrud.update(document, upsert = true,
      new SearchDTDocumentEntityManager(simpleQueryExtractor),
      refreshPolicy = refreshPolicy)

    val docResult: IndexDocumentResult = IndexDocumentResult(index = response.index,
      id = response.id,
      version = response.version,
      created = response.created
    )

    docResult
  }

  def update(indexName: String, document: DTDocumentUpdate, check: Boolean,
             refreshPolicy: RefreshPolicy.Value): UpdateDocumentResult = {
    val indexLanguageCrud = IndexLanguageCrud(elasticClient, indexName)

    if (check) {
      //TODO: to be implemented checks on state, analyzers, actions ...
    }

    val response = indexLanguageCrud.update(document,
      refreshPolicy = refreshPolicy,
      entityManager = new SearchDTDocumentEntityManager(simpleQueryExtractor))

    response
  }

  def getDTDocuments(indexName: String ): SearchDTDocumentsResults = {
    val indexLanguageCrud = IndexLanguageCrud(elasticClient, indexName)
    val query = QueryBuilders.matchAllQuery

    //get a map of stateId -> AnalyzerItem (only if there is smt in the field "analyzer")
    val decisionTableContent = indexLanguageCrud.read(query, version = Option(true), maxItems = Some(10000),
      entityManager = new SearchDTDocumentEntityManager(simpleQueryExtractor))
      .map(_.documents)
      .sortBy(_.document.state)

    val total: Int = decisionTableContent.length
    SearchDTDocumentsResults(total = total, maxScore = .0f, hits = decisionTableContent)
  }

  def read(indexName: String, ids: List[String]): SearchDTDocumentsResults = {
    val indexLanguageCrud = IndexLanguageCrud(elasticClient, indexName)

    val documents = indexLanguageCrud
      .readAll(ids, new SearchDTDocumentEntityManager(simpleQueryExtractor))
      .map(_.documents)

    val maxScore: Float = .0f
    val total: Int = documents.length
    SearchDTDocumentsResults(total = total, maxScore = maxScore, hits = documents)
  }

  private[this] val simpleQueryExtractor: (Map[String, SearchHits], Map[String, Any]) => (List[String], List[List[String]]) = (_: Map[String, SearchHits], source: Map[String, Any]) => {
    source.get("queries") match {
      case Some(t) => (t.asInstanceOf[util.ArrayList[util.HashMap[String, String]]]
        .asScala.map { res => Some(res.getOrDefault("query", None.orNull)) }
        .filter(_.nonEmpty).map(_.get).toList, List.empty[List[String]])
      case None => (List.empty, List.empty[List[String]])
    }
  }

  private[this] val orderedQueryExtractor = (innerHits: Map[String, SearchHits], source: Map[String, Any]) => {
    source.get("queries") match {
      case Some(t) =>
        val offsets = innerHits("queries").getHits.toList.map(innerHit => {
          innerHit.getNestedIdentity.getOffset
        })
        val query_array = t.asInstanceOf[java.util.ArrayList[java.util.HashMap[String, String]]].asScala.toList
          .map(q_e => q_e.get("query"))
        (offsets.map(i => query_array(i)), List.empty[List[String]])
      case None => (List.empty, List.empty[List[String]])
    }
  }

  private[this] val queryAndNgramExtractor = (sliding: Int) => (innerHits: Map[String, SearchHits], source: Map[String, Any]) => {
    source.get("queries") match {
      case Some(t) =>
        val offsetsAndNgrams = innerHits("queries").getHits.toList.map(innerHit => {
          innerHit.getNestedIdentity.getOffset
        })
        val queryArray = t.asInstanceOf[java.util.ArrayList[java.util.HashMap[String, String]]].asScala.toList
          .map(q_e => q_e.get("query"))
        offsetsAndNgrams.map { e =>
          val qNgrams = queryArray(e).toLowerCase().replaceAll("\\s", "").sliding(sliding).toList
          (queryArray(e), qNgrams)
        }.unzip
      case None => (List.empty, List.empty[List[String]])
    }
  }

  def indexCSVFileIntoDecisionTable(indexName: String, file: File,
                                    skipLines: Int = 1, separator: Char = ',',
                                    refreshPolicy: RefreshPolicy.Value): IndexDocumentListResult = {
    val documents: IndexedSeq[DTDocument] = FileToDocuments.getDTDocumentsFromCSV(log = log, file = file,
      skipLines = skipLines, separator = separator)

    bulkCreate(indexName = indexName, documents = documents, refreshPolicy = refreshPolicy)
  }

  def indexJSONFileIntoDecisionTable(indexName: String, file: File,
                                     refreshPolicy: RefreshPolicy.Value): IndexDocumentListResult = {
    val documents: IndexedSeq[DTDocument] = FileToDocuments.getDTDocumentsFromJSON(log = log, file = file)

    bulkCreate(indexName = indexName, documents = documents, check = false, refreshPolicy = refreshPolicy)
  }

  def bulkCreate(indexName: String, documents: IndexedSeq[DTDocument],
                 check: Boolean = true, refreshPolicy: RefreshPolicy.Value): IndexDocumentListResult = {
    val indexLanguageCrud = IndexLanguageCrud(elasticClient, indexName)

    val listOfDocRes = indexLanguageCrud.bulkCreate(documents.toList,
      new SearchDTDocumentEntityManager(simpleQueryExtractor), refreshPolicy)

    IndexDocumentListResult(data = listOfDocRes)
  }

  def wordFrequenciesInQueries(indexName: String): Map[String, Double] = {
    val indexLanguageCrud = IndexLanguageCrud(elasticClient, indexName)

    /*   Query to fetch for word freq in queries
    {
       "size": 0,
       "aggregations" : {
           "wordInQueries": {
               "nested": { "path": "queries" },
               "aggregations": {
                   "queries_children": {
                       "terms" : { "field": "queries.query.base" }
                   }
               }

           }
        }
    } */

    val aggregation = AggregationBuilders.nested("queries", "queries")
      .subAggregation(
        AggregationBuilders.terms("queries_children").field("queries.query.base").minDocCount(1)
      )

    val query = QueryBuilders.matchAllQuery

    val response = indexLanguageCrud.read(query,
      searchType = SearchType.DFS_QUERY_THEN_FETCH,
      requestCache = Some(true),
      maxItems = Option(0),
      minScore = Option(0.0f),
      aggregation = List(aggregation),
      entityManager = WordFrequenciesInQueryEntityManager)

    response.headOption.map(_.frequencies).getOrElse(Map.empty)
  }

  def wordFrequenciesInQueriesByState(indexName: String): List[DTStateWordFreqsItem] = {
    val indexLanguageCrud = IndexLanguageCrud(elasticClient, indexName)

    /*   Query to fetch for each state word freq in queries
    {
       "size": 0,
       "query": {
          "bool": {
            "must": [
              {
                "nested": {
                  "path": "queries",
                  "query": {
                    "bool": {
                      "filter": {
                        "exists": {
                          "field": "queries"
                        }
                      }
                    }
                  }
                }
              }
            ]
          }
        },
        "aggs": {
        "states": {
          "terms": {
            "field": "state",
            "size": 10000000
          },
          "aggs": {
            "queries": {
              "nested": { "path": "queries" },
              "aggregations": {
                "queries_children": {
                  "terms" : { "field": "queries.query.base" }
                }
              }
            }
          }
        }
      }
    } */

    val stateAggsName = "StatesWordStats"

    // Filter all states with queries
    val query = QueryBuilders.boolQuery().must(
      QueryBuilders.nestedQuery("queries",
        QueryBuilders.boolQuery().filter(QueryBuilders.existsQuery("queries")), ScoreMode.None))

    // Calculate for each state with queries the words freq histogram.
    val aggregation = AggregationBuilders.terms(stateAggsName).field("state").size(65536).minDocCount(1)
      .subAggregation(
        AggregationBuilders.nested("queries", "queries")
          .subAggregation(
            AggregationBuilders.terms("queries_children").field("queries.query.base").minDocCount(1)
          )
      )

    indexLanguageCrud.read(query, searchType = SearchType.DFS_QUERY_THEN_FETCH,
      aggregation = List(aggregation),
      requestCache = Some(true),
      maxItems = Option(0),
      minScore = Option(0.0f),
      entityManager = DtStateWordFreqsEntityManager)
  }


  def totalQueriesMatchingRegEx(indexName: String, rx: String): Long = {
    val indexLanguageCrud = IndexLanguageCrud(elasticClient, indexName)

    /*   Query to fetch inner hits for the queries which satisfy regex expression
    {
      "size": 10000,
      "query": {
        "nested": {
          "path": "queries",
          "query": {
            "regexp": {
              "queries.query.base": {
                "value": "acc.*",
                "flags": "ALL",
                "max_determinized_states": 10000,
                "rewrite": "constant_score"
              }
            }
          },
          "inner_hits": {
            "size": 100
          }
        }
      }
    }
    */

    val query: BoolQueryBuilder = QueryBuilders.boolQuery().should(
      QueryBuilders.nestedQuery("queries", QueryBuilders
        .regexpQuery("queries.query.base", rx)
        .maxDeterminizedStates(10000), ScoreMode.None)
        .innerHit(new InnerHitBuilder().setSize(100)) // Increase in Index Definition
    )

    val totalHits = indexLanguageCrud.read(query, searchType = SearchType.DFS_QUERY_THEN_FETCH,
      requestCache = Some(true),
      maxItems = Option(10000),
      minScore = Option(0.0f),
      entityManager = TotalQueriesMatchingEntityManager)

    totalHits.map(_.queriesTotalHits).sum
  }


  def totalQueriesOfStateMatchingRegEx(indexName: String, rx: String, stateName: String): Long = {
    val indexLanguageCrud = IndexLanguageCrud(elasticClient, indexName)

    /*   Query to fetch inner hits for the queries which satisfy regex expression
    "size": 10000,
      "query":
      {
         "bool" : {
            "must": {
              "match": {
                "state": "quickstart"
                }
             },
            "filter": {
              "nested": {
                "path": "queries",
                "query": {
                  "regexp": {
                    "queries.query.base": {
                      "value": "start.*",
                      "flags": "ALL",
                      "max_determinized_states": 10000,
                      "rewrite": "constant_score"
                    }
                  }
                },
                  "inner_hits": {"size": 100}
              }
            }
      }
      }
      }
    */

    val query = QueryBuilders.boolQuery()
      .must(QueryBuilders.matchQuery("state", stateName))
      .filter(QueryBuilders.nestedQuery("queries",
        QueryBuilders.regexpQuery("queries.query.base", rx).maxDeterminizedStates(10000), ScoreMode.None)
        .innerHit(new InnerHitBuilder().setSize(100))) // Increase in Index Definition

    val totalHits = indexLanguageCrud.read(query, searchType = SearchType.DFS_QUERY_THEN_FETCH,
      requestCache = Some(true),
      maxItems = Option(10000),
      minScore = Option(0.0f),
      entityManager = TotalQueriesMatchingEntityManager)

    totalHits.map(_.queriesTotalHits).sum

  }

  def cloneIndexContentReindex(indexNameSrc: String, indexNameDst: String,
                               reset: Boolean = true, propagate: Boolean = true,
                               refreshPolicy: RefreshPolicy.Value
                              ): ReindexResult = {

    val instanceRegistryService: InstanceRegistryService.type = InstanceRegistryService
    if (indexNameSrc === indexNameDst)
      throw DecisionTableServiceException(s"Bad clone operation: " +
        s"src($indexNameSrc) and dst($indexNameDst) are the same")

    // reset the destination index
    if (reset) {
      IndexLanguageCrud(DecisionTableElasticClient, indexNameDst).delete(QueryBuilders.matchAllQuery,
        RefreshPolicy.wait_for)
    }

    // set index and instamce parameters
    val srcInstance: String = Index.instanceName(indexNameSrc)
    val dstInstance: String = Index.instanceName(indexNameDst)
    val sourceIndex: String =
      Index.esLanguageFromIndexName(indexNameSrc, DecisionTableElasticClient.indexSuffix)
    val destinationIndex: String =
      Index.indexName(CloneDtElasticClient.indexName, CloneDtElasticClient.indexSuffix)

    /* reset of data on temporary table */
    val request = new DeleteByQueryRequest(destinationIndex)
    request.setConflicts("proceed")
    request.setQuery(QueryBuilders.termQuery(DecisionTableElasticClient.instanceFieldName, dstInstance))
    log.debug("Delete request: {}", request)
    CloneDtElasticClient.httpClient
      .deleteByQuery(request, RequestOptions.DEFAULT)

    // first reindex with new id and changed instance
    val sourceQueryReq1 = QueryBuilders.termQuery(CloneDtElasticClient.instanceFieldName, srcInstance)
    val reindexReq1: ReindexRequest = new ReindexRequest()
    reindexReq1
      .setSourceIndices(sourceIndex)
      .setDestIndex(destinationIndex)
      .setSourceQuery(sourceQueryReq1)
      .setScript(reindexScript(dstInstance))
      .setDestOpType("create")
      .setRefresh(true)

    val bulkResponse1: BulkByScrollResponse =
      CloneDtElasticClient.httpClient.reindex(reindexReq1, RequestOptions.DEFAULT);

    // second reindex
    val sourceQueryReq2 = QueryBuilders.termQuery("instance", dstInstance)
    val reindexReq2: ReindexRequest = new ReindexRequest()
    reindexReq2
      .setSourceIndices(destinationIndex)
      .setDestIndex(sourceIndex)
      .setSourceQuery(sourceQueryReq2)
      .setDestOpType("create")
      .setRefresh(true)

    val bulkResponse2: BulkByScrollResponse =
      CloneDtElasticClient.httpClient.reindex(reindexReq2, RequestOptions.DEFAULT);

    if(bulkResponse1.getCreated =/= bulkResponse2.getCreated ||
      bulkResponse1.getTotal =/= bulkResponse2.getTotal ||
      bulkResponse1.getDeleted =/= bulkResponse2.getDeleted ||
      bulkResponse1.getUpdated =/= bulkResponse2.getUpdated ||
      bulkResponse1.getVersionConflicts =/= bulkResponse2.getVersionConflicts
    ){
      throw DecisionTableServiceException(s"Error cloning index $indexNameSrc into " +
        s"$indexNameDst")
    }

    if(propagate)
      instanceRegistryService.updateTimestamp(dtIndexName = indexNameDst)

    ReindexResult(
      created = bulkResponse2.getCreated,
      deleted = bulkResponse2.getDeleted,
      updated = bulkResponse2.getUpdated,
      total = bulkResponse2.getTotal,
      versionConflicts = bulkResponse2.getVersionConflicts
    )
  }

  @deprecated("this attribute will be removed, see: cloneIndexContentReindex instead", "StarChat v6.0.0")
  def cloneIndexContent(indexNameSrc: String, indexNameDst: String,
                        reset: Boolean = true, propagate: Boolean = true,
                        refreshPolicy: RefreshPolicy.Value
                       ): IndexDocumentListResult = {
    val instanceRegistryService: InstanceRegistryService.type = InstanceRegistryService
    if (reset) {
      IndexLanguageCrud(elasticClient, indexNameDst).delete(QueryBuilders.matchAllQuery, refreshPolicy)
    }

    if (indexNameSrc === indexNameDst)
      throw DecisionTableServiceException(s"Cloning index is impossible: " +
        s"src($indexNameSrc) and dst($indexNameDst) are the same")

    val srcDocuments = getDTDocuments(indexName = indexNameSrc)
    val documents = srcDocuments.hits.map(_.document).toIndexedSeq
    val createResult = this.bulkCreate(indexName=indexNameDst,
      documents = documents, refreshPolicy = refreshPolicy)

    if(createResult.data.length =/= srcDocuments.total)
      throw DecisionTableServiceException(s"Error cloning index $indexNameSrc into " +
        s"$indexNameDst: ${createResult.data.length} != ${srcDocuments.total}")

    if(propagate)
      instanceRegistryService.updateTimestamp(dtIndexName = indexNameDst)

    createResult
  }

  private[this] def markAsDeletedScriptStr =
    s"""ctx._source.timestamp = ${System.currentTimeMillis()}L ;
       |ctx._source.status = "DELETED" ;""".stripMargin
  private[this] def markAsDeletedScript: Script = new Script(markAsDeletedScriptStr)

  private[this] def markAsDeleted(indexName: String, ids: List[String],
                                  refreshPolicy: RefreshPolicy.Value) : DeleteDocumentsResult = {
    val boolQueryBuilder: BoolQueryBuilder = QueryBuilders.boolQuery()
    //boolQueryBuilder.must(QueryBuilders.termQuery("status", DTDocumentStatus.VALID.toString))
    val orQuery = QueryBuilders.boolQuery()
    ids.foreach { cId => orQuery.should(QueryBuilders.termQuery("state", cId)) }
    boolQueryBuilder.must(orQuery)
    val updateRes: UpdateByQueryResult = IndexLanguageCrud(elasticClient, indexName)
      .updateByQuery(queryBuilder = boolQueryBuilder,
        script = Some(markAsDeletedScript),
        batchSize = None,
        refreshPolicy = refreshPolicy)

    if(updateRes.updatedDocs =/= ids.length.toLong ||
      updateRes.totalDocs =/= updateRes.updatedDocs ||
      updateRes.timedOut)
      throw DecisionTableServiceException(s"Errors marking documents as Deleted: Tot(${updateRes.totalDocs}) Updated(${updateRes.updatedDocs}) Ids(${ids.length.toLong})")

    DeleteDocumentsResult(
      data = ids.map(id => {
        DeleteDocumentResult(
          index = indexName,
          id = id,
          version = 0,
          found = true)
      })
    )
  }

  private[this] def markAsDeletedAll(indexName: String,
                                     refreshPolicy: RefreshPolicy.Value) : DeleteDocumentsSummaryResult = {
    val updateRes: UpdateByQueryResult = IndexLanguageCrud(elasticClient, indexName)
      .updateByQuery(queryBuilder = QueryBuilders.matchAllQuery,
        script = Some(markAsDeletedScript),
        batchSize = None,
        refreshPolicy = refreshPolicy)
    DeleteDocumentsSummaryResult(
      message = "delete",
      deleted = updateRes.updatedDocs
    )
  }

  override def delete(indexName: String, ids: List[String],
                      refreshPolicy: RefreshPolicy.Value): DeleteDocumentsResult = {
    val indexLanguageCrud = IndexLanguageCrud(elasticClient, indexName)
    val response = indexLanguageCrud.delete(ids, refreshPolicy, EmptyWriteEntityManager)

    DeleteDocumentsResult(data = response)
  }

  override def deleteAll(indexName: String, refreshPolicy: RefreshPolicy.Value): DeleteDocumentsSummaryResult = {
    val indexLanguageCrud = IndexLanguageCrud(elasticClient, indexName)
    val response = indexLanguageCrud.delete(QueryBuilders.matchAllQuery, refreshPolicy)

    DeleteDocumentsSummaryResult(message = "delete", deleted = response.getTotal)
  }

  def deleteOrMark(indexName: String, ids: List[String],
                   refreshPolicy: RefreshPolicy.Value,
                   permament: Boolean = false): DeleteDocumentsResult = {
    if(permament)
      delete(indexName, ids, refreshPolicy)
    else
      markAsDeleted(indexName, ids, refreshPolicy)
  }

  def deleteOrMarkAll(indexName: String, refreshPolicy: RefreshPolicy.Value,
                      permament: Boolean = false): DeleteDocumentsSummaryResult = {
    if(permament)
      deleteAll(indexName, refreshPolicy)
    else
      markAsDeletedAll(indexName, refreshPolicy)
  }

}