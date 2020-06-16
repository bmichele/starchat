package com.getjenny.starchat.services

/**
 * Created by michele.boggia@getjenny.com on 05/05/20.
 */

import com.getjenny.starchat.entities.io._
import com.getjenny.starchat.services.esclient.KnowledgeBaseElasticClient
import com.getjenny.starchat.utils.Index
import org.elasticsearch.action.search.{SearchRequest, SearchResponse}
import org.elasticsearch.client.{RequestOptions, RestHighLevelClient}
import org.elasticsearch.index.query.QueryBuilders
import org.elasticsearch.search.aggregations.AggregationBuilders
import org.elasticsearch.search.aggregations.bucket.filter.FilterAggregationBuilder
import spray.json._
import org.elasticsearch.search.builder.SearchSourceBuilder
import org.elasticsearch.search.suggest.SuggestBuilder
import org.elasticsearch.search.suggest.term.TermSuggestionBuilder.{StringDistanceImpl, SuggestMode}
import org.elasticsearch.search.suggest.term.{TermSuggestion, TermSuggestionBuilder}

import scala.collection.JavaConverters._
import scala.collection.immutable.List
import scala.math.Ordering.Float.equiv


object SpellcheckService2 extends AbstractDataService {
  override val elasticClient: KnowledgeBaseElasticClient.type = KnowledgeBaseElasticClient
  val indexSuffixLogs = "logs_data"
  val (labelT, labelLT, labelLLT, labelTR, labelTRR, labelLTR) = ("t", "lt", "llt", "tr", "trr", "ltr")
  val (labelUnigrams, labelBigrams, labelTrigrams) = ("unigrams", "bigrams", "trigrams")
  val (labelQuestionUnigrams, labelQuestionBigrams, labelQuestionTrigrams) = ("question.base", "question.shingles_2", "question.shingles_3")
  val (aggregationPrefixFilter, aggregationPrefixValue) = ("filter#", "value_count#")
  val (aggregationFieldDocCount, aggregationFieldValue) = ("doc_count", "value")

  /**
   * Function to get suggestions from ES suggester, given a sentence.
   * @param indexName name of the index
   * @param request e.g. SpellcheckTermsRequest2(text = "Is this setnence misspelled?")
   * @return list of SpellCheckToken2 objects
   */
  def suggestionsES(indexName: String, request: SpellcheckTermsRequest2) : List[SpellcheckToken] = {
    val esLanguageSpecificIndexName = Index.esLanguageFromIndexName(indexName, indexSuffixLogs)
    val client: RestHighLevelClient = elasticClient.httpClient

    val suggestionBuilder: TermSuggestionBuilder = new TermSuggestionBuilder(labelQuestionUnigrams)
    suggestionBuilder.maxEdits(request.maxEdit)
      .prefixLength(request.prefixLength)
      .minDocFreq(request.minDocFreq)
      .minWordLength(request.minWordLength)
      .suggestMode(SuggestMode.MISSING)
      .stringDistance(StringDistanceImpl.DAMERAU_LEVENSHTEIN)
      .size(100)

    val suggestBuilder: SuggestBuilder = new SuggestBuilder()
    val labelSuggestions = "suggestions"
    suggestBuilder.setGlobalText(request.text)
      .addSuggestion(labelSuggestions, suggestionBuilder)

    val sourceReq: SearchSourceBuilder = new SearchSourceBuilder()
      .suggest(suggestBuilder)

    val searchReq = new SearchRequest(esLanguageSpecificIndexName)
      .source(sourceReq)

    val searchResponse : SearchResponse = client.search(searchReq, RequestOptions.DEFAULT)

    val termsSuggestions: List[SpellcheckToken] =
      searchResponse.getSuggest.getSuggestion[TermSuggestion](labelSuggestions)
        .getEntries.asScala.toList.map {suggestions =>
        val item: TermSuggestion.Entry = suggestions
        val text = item.getText.toString
        val offset = item.getOffset
        val length = item.getLength
        val options: List[SpellcheckTokenSuggestions] =
          item.getOptions.asScala.toList.map { suggestion =>
            val option = SpellcheckTokenSuggestions(
              score = suggestion.getScore.toDouble,
              freq = suggestion.getFreq.toDouble,
              text = suggestion.getText.toString
            )
            option
          }
        val spellcheckToken =
          SpellcheckToken(text = text, offset = offset, length = length,
            options = options)
        spellcheckToken
      }
    termsSuggestions
  }

  /**
   * Helper function for SpellcheckService2.ngramCounts.
   * Create a `FilterAggregationBuilder` object used to retrieve the number of occurrences of a given token, equivalent to
   *
   * <name>: {
   *   "filter": {
   *     "match": {
   *       "question.base": <token>
   *     }
   *   }
   * }
   *
   * @param name name assigned to the aggregation
   * @param token searched string
   * @return FilterAggregationBuilder object to be used in search request
   */
  private[this] def aggregationUnigramOccurrences(name: String, token: String): FilterAggregationBuilder =
    AggregationBuilders
      .filter(name, QueryBuilders.termQuery(labelQuestionUnigrams, token))

  /**
   * Helper functions to combine strings
   * @param tokens list of strings
   * @return string composed by input strings, separated by whitespace or underscore
   */
  private[this] def combineTokensWhitespace(tokens: List[String]) = tokens.foldRight("")((x, y) => x + " " + y).dropRight(1)
  private[this] def combineTokensUnderscore(tokens: List[String]) = tokens.foldRight("")((x, y) => x + "_" + y).dropRight(1)

  /**
   * Helper function for SpellcheckService2.ngramCounts.
   * Similar to SpellcheckService2.aggregationUnigramOccurrences, for bigrams.
   * @param name name assigned to the aggregation
   * @param tokens tuple containing the bigram tokens
   * @return FilterAggregationBuilder object to be used in search request
   */
  private[this] def aggregationBigramOccurrences(name: String, tokens: (String, String)): FilterAggregationBuilder = {
    val text = tokens match { case (token1, token2) => combineTokensWhitespace(List(token1, token2))}
    AggregationBuilders
      .filter(name, QueryBuilders.termQuery(labelQuestionBigrams, text))
  }

  /**
   * Helper function for SpellcheckService2.ngramCounts.
   * Similar to SpellcheckService2.aggregationUnigramOccurrences, for trigrams.
   * @param name name assigned to the aggregation
   * @param tokens tuple containing the trigram tokens
   * @return FilterAggregationBuilder object to be used in search request
   */
  private[this] def aggregationTrigramOccurrences(name: String, tokens: (String, String, String)): FilterAggregationBuilder = {
    val text = tokens match {
      case (token1, token2, token3) => combineTokensWhitespace(List(token1, token2, token3))
    }
    AggregationBuilders
      .filter(name, QueryBuilders.termQuery(labelQuestionTrigrams, text))
  }

  /**
   * Helper function for SpellcheckService2.ngramCounts.
   * Given a token, its left context (word_m2, word_m1) and its right context (word_p1, word_p2), add to the input
   * `SearchSourceBuilder` object the following named aggregations:
   *  - "agg_<index>_t": number of occurrences of <candidate>
   *  - "agg_<index>_lt": number of occurrences of the sequence word_m1, <candidate> (if word_m1 is present)
   *  - "agg_<index>_llt": number of occurrences of the sequence word_m2, word_m1, <candidate> (if word_m1, word_m2 are present)
   *  - "agg_<index>_tr": number of occurrences of the sequence <candidate>, word_p1 (if word_p1 is present)
   *  - "agg_<index>_trr": number of occurrences of the sequence <candidate>, word_p1, word_p2 (if word_p1, word_p2 are present)
   *  - "agg_<index>_ltr": number of occurrences of the sequence word_m1, <candidate>, word_p1 (if word_m1, word_p1 are present)
   * @param searchSourceBuilder `SearchSourceBuilder` input object
   * @param candidate candidate token
   * @param index integer index, used to identify the aggregations added to `searchSourceBuilder`
   * @param leftContext words preceding the candidate, e.g. List(word_m2, word_m1) in the example above
   * @param rightContext words following the candidate, e.g. List(word_p1, word_p2) in the example above
   * @return `SearchSourceBuilder` object obtained from `searchSourceBuilder` by adding aggregations listed above
   */
  private[this] def aggregationsSingleCandidate(
                                           searchSourceBuilder: SearchSourceBuilder,
                                           candidate: String,
                                           index: Int,
                                           leftContext: List[String],
                                           rightContext: List[String]
                                         ): SearchSourceBuilder = {
    val indexString = index.toString
    val leftContextReverse = leftContext.reverse
    val aggT = aggregationUnigramOccurrences(
      combineTokensUnderscore(List(indexString, labelT)),
      candidate
    )
    searchSourceBuilder.aggregation(aggT)
    if (leftContext.nonEmpty) {
      val aggLT = aggregationBigramOccurrences(combineTokensUnderscore(List(indexString, labelLT)), (leftContextReverse(0), candidate))
      searchSourceBuilder.aggregation(aggLT)
      if (leftContext.size > 1)  {
        val aggLLT = aggregationTrigramOccurrences(combineTokensUnderscore(List(indexString, labelLLT)), (leftContextReverse(1), leftContextReverse(0), candidate))
        searchSourceBuilder.aggregation(aggLLT)
      }
      if (rightContext.nonEmpty) {
        val aggLTR = aggregationTrigramOccurrences(combineTokensUnderscore(List(indexString, labelLTR)), (leftContextReverse(0), candidate, rightContext(0)))
        searchSourceBuilder.aggregation(aggLTR)
      }
    }
    if (rightContext.nonEmpty) {
      val aggTR = aggregationBigramOccurrences(combineTokensUnderscore(List(indexString, labelTR)), (candidate, rightContext(0)))
      searchSourceBuilder.aggregation(aggTR)
      if (rightContext.size > 1) {
        val aggTRR = aggregationTrigramOccurrences(combineTokensUnderscore(List(indexString, labelTRR)), (candidate, rightContext(0), rightContext(1)))
        searchSourceBuilder.aggregation(aggTRR)
      }
    }
    searchSourceBuilder
  }

  /**
   * Helper function for SpellcheckService2.ngramCounts.
   * Allows to retrieve aggregation results from ES response. For example, when the following aggregation is present in the response object
   * {
   *   "aggregations":{
   *     <aggregationName>:{
   *       <field>:42
   *     }
   *   }
   * }
   * aggregationValue returns 42
   * @param responseElasticsearch `SearchResponse` object containing aggregation
   * @param aggregationName name assigned to aggregation
   * @param field field in aggregation from which values is extracted
   * @return value corresponding to aggregations.aggregationName.field
   */
  private[this] def aggregationValue(responseElasticsearch: SearchResponse, aggregationName: String, field: String): Int = {
    // TODO: refactor - here I extract values converting ES response to json, it should be doable with ES APIs
    def fieldAsJsObject(json: JsObject, fieldName: String): JsObject =
      json.getFields(fieldName)(0).asJsObject
    val responseJson = responseElasticsearch.toString.parseJson.asJsObject
    val aggregationsJson = fieldAsJsObject(responseJson, "aggregations")
    val aggregationJson = fieldAsJsObject(aggregationsJson, aggregationName)
    aggregationJson.getFields(field)(0).toString.toInt
  }

  /**
   * True function takes a list of candidates, together with left and right context, and returns all necessary counts in
   * two maps, e.g.
   *
   * ngramCounts(["cand_1", "cand_2"],
   *             ["word_m2", "word_m1"],
   *             ["word_p1", "word_p2"])
   *  -> (
   *       Map("cand1" -> Map("t" -> 123, "lt" -> 54, "llt" -> 23, "tr" -> 67, "trr" -> 42, "ltr" -> 15),
   *           "cand2" -> Map("t" -> 1243, "lt" -> 354, "llt" -> 123, "tr" -> 670, "trr" -> 423, "ltr" -> 123)),
   *       Map("unigrams" -> 654654,
   *           "bigrams" -> 234324,
   *           "trigrams" -> 53434)
   *     )
   *
   * The output values given for each candidates correspond to:
   *  - "t": count of token "cand_i"
   *  - "lt": count of bigram "word_m1 cand_i"
   *  - "llt": count of trigram "word_m2 word_m1 cand_i"
   *  - "tr": count of bigram "cand_i word_p1"
   *  - "trr": count of trigram "cand_i word_p1 word_p2"
   *  - "ltr": count of trigram "word_m1 cand_i word_p1"
   * The second map object contains:
   *  - "unigrams": total number of unigrams (counting multiple occurrences)
   *  - "bigrams": total number of bigrams (counting multiple occurrences)
   *  - "trigrams": total number of trigrams (counting multiple occurrences)
   */
  def ngramCounts(indexName: String,
                  candidates: List[String],
                  leftContext: List[String],
                  rightContext: List[String]): (Map[String, Map[String, Int]], Map[String, Int]) = {

    // initialize SearchSourceBuilder object to prepare ES request
    val sourceReqBuilder: SearchSourceBuilder = new SearchSourceBuilder().size(0)
    // add to request, for every candidate, all necessary aggregations by using aggregationsSingleCandidate function
    def helperFunction(candidateIndex: (String, Int), accumulator: SearchSourceBuilder): SearchSourceBuilder = {
      candidateIndex match {
        case (candidate, index) =>
          aggregationsSingleCandidate (accumulator, candidate, index, leftContext, rightContext)
      }
    }
    val sourceReqBuilderCandidates = candidates.zipWithIndex.foldRight(sourceReqBuilder)(helperFunction)
    // add to request aggregations to get total number of unigrams, bigrams and trigrams
    val aggregationTotal1Grams = AggregationBuilders.count(labelUnigrams).field(labelQuestionUnigrams)
    val aggregationTotal2Grams = AggregationBuilders.count(labelBigrams).field(labelQuestionBigrams)
    val aggregationTotal3Grams = AggregationBuilders.count(labelTrigrams).field(labelQuestionTrigrams)
    sourceReqBuilderCandidates
      .aggregation(aggregationTotal1Grams)
      .aggregation(aggregationTotal2Grams)
      .aggregation(aggregationTotal3Grams)

    // perform search
    val esLanguageSpecificIndexName = Index.esLanguageFromIndexName(indexName, indexSuffixLogs)
    val client: RestHighLevelClient = elasticClient.httpClient
    val searchReq: SearchRequest = new SearchRequest(esLanguageSpecificIndexName)
      .source(sourceReqBuilderCandidates)
    val searchResponse : SearchResponse = client.search(searchReq, RequestOptions.DEFAULT)

    // extract values from response and build output map
    def buildCandidateStats(candidateIndex: Int): Map[String, Int] = {
      val indexString = candidateIndex.toString

      def namedAggregation(label: String): (String, Int) =
        (label, aggregationValue(searchResponse, aggregationPrefixFilter + combineTokensUnderscore(List(indexString, label)), aggregationFieldDocCount))

      val outList = (leftContext, rightContext) match {
        case ( _ :: tailLeft, _ :: tailRight) =>
          tailLeft match {
            case Nil => tailRight match { // only one element in leftC
              case Nil => List(labelLT, labelT, labelTR, labelLTR) // one element in leftC and rightC
              case _ => List(labelLT, labelT, labelTR, labelLTR, labelTRR) // one element in leftC and two elements in rightC
            }
            case _ => tailRight match { // two elements in leftC
              case Nil => List(labelLT, labelT, labelTR, labelLLT, labelLTR) // two elements in leftC and one in rightC
              case _ => List(labelLT, labelT, labelTR, labelLLT, labelLTR, labelTRR) // two elements in leftC and rightC
            }
          }
        case (Nil,  _ :: tailRight) =>
          tailRight match { // no elements in leftC
            case Nil => List(labelT, labelTR) // no elements in leftC and one in rightC
            case _ => List(labelT, labelTR, labelTRR) // no elements in leftC and two in rightC
          }
        case (_ :: tailLeft, Nil) =>
          tailLeft match { // no elements in rightC
            case Nil => List(labelLT, labelT) // no elements in rightC and one in leftC
            case _ => List(labelLT, labelT, labelLLT) // no elements in rightC and two in leftC
          }
        case (Nil, Nil) => List(labelT)
      }
      outList.map(namedAggregation).toMap
    }
    val candidateCounts = candidates.zipWithIndex.map {
      case (candidate, index) => (candidate, buildCandidateStats(index))
    }.toMap
    val ngramTotalCounts = Map(
      labelUnigrams -> aggregationValue(searchResponse, aggregationPrefixValue + labelUnigrams, aggregationFieldValue),
      labelBigrams -> aggregationValue(searchResponse, aggregationPrefixValue + labelBigrams, aggregationFieldValue),
      labelTrigrams -> aggregationValue(searchResponse, aggregationPrefixValue + labelTrigrams, aggregationFieldValue),
    )
    (candidateCounts, ngramTotalCounts)
  }

  /**
   * Function to combine n-gram scores. n-gram scores (for n = 1, 2, 3) corresponds to S_1, S_2, S_3 as defined in
   * eq.(2) of https://arxiv.org/abs/1910.11242 (before applying weights)
   * @param scores1 list containing unweighted unigram scores
   * @param scores2 list containing unweighted bigram scores
   * @param scores3 list containing unweighted trigram scores
   * @param weightUnigrams unigram score weight for final score computation
   * @param weightBigrams bigram score weight for final score computation
   * @param weightTrigrams trigram score weight for final score computation
   * @return
   */
  private[this] def combineScores(
                                   scores1: List[Float],
                                   scores2: List[Float],
                                   scores3: List[Float],
                                   weightUnigrams: Float,
                                   weightBigrams: Float,
                                   weightTrigrams: Float
                                 ): List[Float] =
    (
      scores1.map(_ * weightUnigrams),
      scores2.map(_ * weightBigrams),
      scores3.map(_ * weightTrigrams)
      ).zipped.map( _ + _ + _ )

  /**
   * Alternative function to combine n-gram scores. See combineScores documentation
   */
  private[this] def combineScoresAlternative(
                                              scores1: List[Float],
                                              scores2: List[Float],
                                              scores3: List[Float],
                                              weightUnigrams: Float,
                                              weightBigrams: Float,
                                              weightTrigrams: Float
                                            ): List[Float] = {
    def rescale(list: List[Float]): List[Float] = {
      val norm = list.map(_.abs).sum  // abs should be redundant, but it's more safe to have it
      list.map {
        x => if (equiv(norm, 0)) 0 else x/norm
      }
    }
    (
      rescale(scores1).map(_ * weightUnigrams),
      rescale(scores2).map(_ * weightBigrams),
      rescale(scores3).map(_ * weightTrigrams)
    ).zipped.map(_ + _ + _).map(_ / 3.toFloat)
  }

  /**
   * Computes, given a list of candidates and the context around the misspelled word, scores for each candidate.
   * For example
   *
   * scoreCandidates(["you", "lol"],
   *                 ["how", "are"],
   *                 ["doing", "today"])
   * -> Map(
   *      "you" -> (0.98, (0.99, 0.32, 0.02))
   *      "lol" -> (0.02, (0.53, 0.02, 0.00))
   *    )
   *
   * @param candidates list of candidates
   * @param leftContext words preceding the misspelled word (at most two)
   * @param rightContext words following the misspelled word (at most two)
   * @param weightUnigrams weight for unigram in final score
   * @param weightBigrams weight for bigrams in final score
   * @param weightTrigrams weight for trigrams in final score
   * @return Map containing, for each candidate the scores: (final score, (s1, s2, s3))
   */
  def scoreCandidates(indexName: String,
                      candidates: List[String],
                      leftContext: List[String],
                      rightContext: List[String],
                      weightUnigrams: Float,
                      weightBigrams: Float,
                      weightTrigrams: Float): Map[String, (Float, (Float, Float, Float))] = {
    // get stats from ES
    val (candidateCounts, ngramCountsRes) = ngramCounts(indexName, candidates, leftContext, rightContext)
    def sumValues(mapObject: Map[String, Int], keys: List[String]): Int = {
      mapObject.filterKeys(keys.contains).values.sum
    }

    // create map with ngram scores for each candidate Map("cand1" -> (s11, s12, s13), "cand2" -> (s21, s22, s23), ...)
    val scoresNgrams = candidateCounts.map {
      case (candidate, counts) =>
        val s1 = counts.getOrElse(labelT, 0).toFloat / ngramCountsRes.getOrElse(labelUnigrams, 1).toFloat
        val s2 = sumValues(counts, List(labelLT, labelTR)).toFloat / ngramCountsRes.getOrElse(labelBigrams, 1).toFloat
        val s3 = sumValues(counts, List(labelLLT, labelLTR, labelTRR)).toFloat / ngramCountsRes.getOrElse(labelTrigrams, 1).toFloat
        (candidate, (s1, s2, s3))
    }
    // combine scores
    def reshapeListTuples3(x: (Float, Float, Float), l: (List[Float], List[Float], List[Float])) =
      (x, l) match {
        case ((x1, x2, x3), (l1, l2, l3)) => (x1 :: l1, x2 :: l2, x3 :: l3)
      }
    val scoresNgramsLists = scoresNgrams.values.foldRight[(List[Float], List[Float], List[Float])]((List(), List(), List()))(reshapeListTuples3)

    // val scoresTotal = combineScores(
    val (scoresTotal, scoreNgramsMatrix) = scoresNgramsLists match {
      case (unigramScoreList, bigramScoreList, trigramScoreList) =>
        (
          combineScoresAlternative(unigramScoreList, bigramScoreList, trigramScoreList, weightUnigrams, weightBigrams, weightTrigrams),
          (unigramScoreList, bigramScoreList, trigramScoreList).zipped.map((_, _, _))
        )
    }
    scoresNgrams.keys.zip(
      scoresTotal.zip(scoreNgramsMatrix)
    ).toMap
  }

  /**
   * Given a list of objects, returns a list containing, for each element, the two elements on the right of the element
   * (if they are present)
   *
   * rightContextList(List(1, 2, 3, 4))
   * -> List(
   *      List(2, 3),
   *      List(3, 4),
   *      List(4),
   *      List()
   *    )
   */
  def rightContextList[T](tokens: List[T]): List[List[T]] = {
    @scala.annotation.tailrec
    def rightContextListAcc(tokenList: List[T], acc: List[List[T]]): List[List[T]] = {
      tokenList match {
        case _ :: Nil => List(List())
        case _ :: t2 :: Nil => acc ++ List(List(t2), List())
        case _ :: t2 :: t3 :: tail => rightContextListAcc(t2 :: t3 :: tail, acc ++ List(List(t2, t3)))
      }
    }
    rightContextListAcc(tokens, List())
  }

  /**
   * As rightContextList, but returns elements on the left
   *
   * leftContextList(List(1, 2, 3, 4))
   * -> List(
   *      List(),
   *      List(1),
   *      List(1, 2),
   *      List(2, 3)
   *    )
   */
  def leftContextList[T](tokens: List[T]): List[List[T]] = {
    rightContextList(tokens.reverse).map(_.reverse).reverse
  }

  /**
   * Helper function for rightProperContextList and leftProperContextList
   */
  private[this] def filterRightContext(list: List[SpellcheckToken]): List[SpellcheckToken] = {
    def filterRightAcc(list: List[SpellcheckToken], acc: List[SpellcheckToken]): List[SpellcheckToken] = {
      list match {
        case Nil => acc
        case head :: tail => if (head.options.isEmpty) head :: filterRightAcc(tail, acc) else acc
      }
    }
    filterRightAcc(list, List())
  }

  /**
   * Given a list of right contexts, cut out context tokens starting from those SpellcheckToken2 object which are misspelled
   */
  def rightProperContextList(rightContextList: List[List[SpellcheckToken]]): List[List[SpellcheckToken]] =
    rightContextList.map(filterRightContext)

  /**
   * Given a list of left contexts, cut out context tokens until the first SpellcheckToken2 object which is not misspelled
   */
  def leftProperContextList(leftContextList: List[List[SpellcheckToken]]): List[List[SpellcheckToken]] = {
    def filterLeftContext(list: List[SpellcheckToken]): List[SpellcheckToken] = {
      filterRightContext(list.reverse).reverse
    }
    leftContextList.map(filterLeftContext)
  }

  def termsSuggester2(indexName: String, request: SpellcheckTermsRequest2) : SpellcheckTermsResponse2 = {
    val res = if (request.text.isEmpty) {
      List()
    }
    else {
      val suggestions = suggestionsES(indexName, request)
      // define list containing, for each token, the corresponding context (including possibly misspelled words)
      val rightContextsUnfiltered = rightContextList(suggestions)
      val leftContextsUnfiltered = leftContextList(suggestions)
      // remove from context misspelled words
      val rightContexts = rightProperContextList(rightContextsUnfiltered)
      val leftContexts = leftProperContextList(leftContextsUnfiltered)
      // list containing, for each token, the possible candidates
      val candidatesLists = suggestions.map(_.options.map(x => (x.text, x.freq)).toMap)
      // zip with contexts to be used in scoring function
      val zipped = (
        candidatesLists,
        rightContexts.map(_.map(_.text)),
        leftContexts.map(_.map(_.text))
        ).zipped.toList
      zipped.zip(suggestions).map {
        case ((tokenCandidates, tokenRightContext, tokenLeftContext), suggestionToken) =>
          if (tokenCandidates.nonEmpty) {
            val scoreCandidatesToken = scoreCandidates(
              indexName,
              tokenCandidates.keys.toList,
              tokenLeftContext,
              tokenRightContext,
              1.toFloat, 1.toFloat, 1.toFloat
            )
            SpellcheckToken2(
              text = suggestionToken.text,
              offset = suggestionToken.offset,
              length = suggestionToken.length,
              options = scoreCandidatesToken.map {
                case (text, (scoreFinal, (scoreUnigrams, scoreBigrams, scoreTrigrams))) =>
                  SpellcheckTokenSuggestions2(
                    score = scoreFinal,
                    scoreUnigram = scoreUnigrams,
                    scoreBigram = scoreBigrams,
                    scoreTrigram = scoreTrigrams,
                    freq = tokenCandidates.getOrElse(text, 0.0),
                    text = text
                  )
              }.toList
            )
          }
          else {
            SpellcheckToken2(
              text = suggestionToken.text,
              offset = suggestionToken.offset,
              length = suggestionToken.length,
              options = suggestionToken.options.map(
                x => SpellcheckTokenSuggestions2(
                  score = x.score,
                  scoreUnigram = 0.0,
                  scoreBigram = 0.0,
                  scoreTrigram = 0.0,
                  freq = x.freq,
                  text = x.text
                )
              )
            )
          }
      }
    }
    SpellcheckTermsResponse2(tokens = res)
  }
}

/*
object Main2 extends App {

  val leftC = List()
  val rightC = List("right1", "right2")

  val outBufferList = new ListBuffer[String]()

  outBufferList += "T"
  if (leftC.nonEmpty) {
    outBufferList += "LT"
    if (leftC.size > 1) outBufferList += "LLT"
    if (rightC.nonEmpty) outBufferList += "LTR"
  }
  if (rightC.nonEmpty) {
    outBufferList += "TR"
    if (rightC.size > 1) outBufferList += "TRR"
  }
  println(outBufferList)

  val outList = (leftC, rightC) match {
    case ( _ :: tailLeft, _ :: tailRight) =>
      tailLeft match {
        case Nil => tailRight match { // only one element in leftC
          case Nil => List("LT", "T", "TR", "LTR") // one element in leftC and rightC
          case _ => List("LT", "T", "TR", "LTR", "TRR") // one element in leftC and two elements in rightC
        }
        case _ => tailRight match { // two elements in leftC
          case Nil => List("LT", "T", "TR", "LLT", "LTR") // two elements in leftC and one in rightC
          case _ => List("LT", "T", "TR", "LLT", "LTR", "TRR") // two elements in leftC and rightC
        }
      }
    case (Nil,  _ :: tailRight) =>
      tailRight match { // no elements in leftC
        case Nil => List("T", "TR") // no elements in leftC and one in rightC
        case _ => List("T", "TR", "TRR") // no elements in leftC and two in rightC
      }
    case (_ :: tailLeft, Nil) =>
      tailLeft match { // no elements in rightC
        case Nil => List("LT", "T") // no elements in rightC and one in leftC
        case _ => List("LT", "T", "LLT") // no elements in rightC and two in leftC
      }
    case (Nil, Nil) => List("T")
  }
  println(outList)
}*/
