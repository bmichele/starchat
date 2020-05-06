package com.getjenny.starchat.services

/**
 * Created by michele.boggia@getjenny.com on 05/05/20.
 */

import com.getjenny.starchat.entities.io._
import com.getjenny.starchat.services.esclient.KnowledgeBaseElasticClient
import com.getjenny.starchat.utils.Index
import org.elasticsearch.action.search.{SearchRequest, SearchResponse}
import org.elasticsearch.client.{RequestOptions, RestHighLevelClient}
import org.elasticsearch.search.builder.SearchSourceBuilder
import org.elasticsearch.search.suggest.SuggestBuilder
import org.elasticsearch.search.suggest.term.TermSuggestionBuilder.{StringDistanceImpl, SuggestMode}
import org.elasticsearch.search.suggest.term.{TermSuggestion, TermSuggestionBuilder}

import scala.collection.JavaConverters._
import scala.collection.immutable.List
import scala.collection.mutable.ListBuffer

case class Token(text: String,
                 offset: Int,
                 length: Int,
                 isMisspelled: Boolean,
                 var isCorrected: Boolean = false,
                 var leftContext: List[Token] = List(),
                 var rightContext: List[Token] = List(),
                 var contextSize: Int = 0,
                 var properContextSize: Int = 0,
                 var options: List[SpellcheckTokenSuggestions2]){
  def setLeftContext(contextLeft: List[Token]): Unit =
    leftContext = contextLeft
  def setRightContext(contextRight: List[Token]): Unit =
    rightContext = contextRight
  def setOptions(suggestions: List[SpellcheckTokenSuggestions2]): Unit =
    options = suggestions
  def updateContextSize(): Unit =
    contextSize = leftContext.size + rightContext.size
  private def getProperContext(context: List[Token], acc: List[Token]): List[Token] = {
    context match {
      case Nil => acc
      case head :: tail => if (head.isMisspelled) acc else head :: getProperContext(tail, acc)
    }
  }
  def getProperContextRight: List[Token] = getProperContext(rightContext, List())
  def getProperContextLeft: List[Token] = getProperContext(leftContext.reverse, List()).reverse
  def updateProperContextSize(): Unit = getProperContextRight.size + getProperContextLeft.size
  def getBestCandidate: String =
    if (isMisspelled) options.maxBy(_.score).text
    else text
}

object SpellcheckService2 extends AbstractDataService {
  override val elasticClient: KnowledgeBaseElasticClient.type = KnowledgeBaseElasticClient

  /**
   * Function to get suggestions from ES suggester, given a sentence.
   * @param indexName name of the index
   * @param request e.g. SpellcheckTermsRequest2(text = "Is this setnence misspelled?")
   * @return list of SpellCheckToken2 objects
   */
  def getSuggestions(indexName: String, request: SpellcheckTermsRequest2) : List[SpellcheckToken2] = {
    val esLanguageSpecificIndexName = Index.esLanguageFromIndexName(indexName, "logs_data")
    val client: RestHighLevelClient = elasticClient.httpClient

    val suggestionBuilder: TermSuggestionBuilder = new TermSuggestionBuilder("question.base")
    suggestionBuilder.maxEdits(request.maxEdit)
      .prefixLength(request.prefixLength)
      .minDocFreq(request.minDocFreq)
      .minWordLength(request.minWordLength)
      .suggestMode(SuggestMode.MISSING)
      .stringDistance(StringDistanceImpl.DAMERAU_LEVENSHTEIN)
      .size(100)

    val suggestBuilder: SuggestBuilder = new SuggestBuilder()
    suggestBuilder.setGlobalText(request.text)
      .addSuggestion("suggestions", suggestionBuilder)

    val sourceReq: SearchSourceBuilder = new SearchSourceBuilder()
      .suggest(suggestBuilder)

    val searchReq = new SearchRequest(esLanguageSpecificIndexName)
      .source(sourceReq)

    val searchResponse : SearchResponse = client.search(searchReq, RequestOptions.DEFAULT)

    val termsSuggestions: List[SpellcheckToken2] =
      searchResponse.getSuggest.getSuggestion[TermSuggestion]("suggestions")
        .getEntries.asScala.toList.map { suggestions =>
        val item: TermSuggestion.Entry = suggestions
        val text = item.getText.toString
        val offset = item.getOffset
        val length = item.getLength
        val options: List[SpellcheckTokenSuggestions2] =
          item.getOptions.asScala.toList.map { suggestion =>
            val option = SpellcheckTokenSuggestions2(
              score = suggestion.getScore.toDouble,
              freq = suggestion.getFreq.toDouble,
              text = suggestion.getText.toString
            )
            option
          }
        val spellcheckToken =
          SpellcheckToken2(text = text, offset = offset, length = length,
            options = options)
        spellcheckToken
      }
    termsSuggestions
  }

  /**
   * Mockup function!!!
   * True function takes a list of candidates, together with left and right context, and returns all necessary counts in
   * two maps, e.g.
   *
   * getStats(["cand_1", "cand_2"],
   *          ["word_m2", "word_m1"],
   *          ["word_p1", "word_p2"])
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
  def getStats(candidates: List[String],
               leftContext: List[String],
               rightContext: List[String]): (Map[String, Map[String, Int]], Map[String, Int]) = {
    def buildCandidateStats(): Map[String, Int] = {
      val combinations = new ListBuffer[String]()
      combinations += "t"
      if (leftContext.nonEmpty) {
        combinations += "lt"
        if (leftContext.size > 1)  combinations += "llt"
        if (rightContext.nonEmpty) combinations += "ltr"
      }
      if (rightContext.nonEmpty) {
        combinations += "tr"
        if (rightContext.size > 1) combinations += "trr"
      }
      combinations.map(x => (x, 10)).toMap
    }
    val candidateCounts = candidates.zipWithIndex.map(pair => (pair._1, buildCandidateStats())).toMap
    val ngramTotalCounts = Map("unigrams" -> 100, "bigrams" -> 50, "trigrams" -> 20)
    (candidateCounts, ngramTotalCounts)
  }

  def combineScores(scores1: List[Float],
                    scores2: List[Float],
                    scores3: List[Float],
                    weightUnigrams: Float,
                    weightBigrams: Float,
                    weightTrigrams: Float): List[Float] =
    (
      scores1.map(_ * weightUnigrams),
      scores2.map(_ * weightBigrams),
      scores3.map(_ * weightTrigrams)
      ).zipped.map( _ + _ + _ )

  def combineScoresAlternative(scores1: List[Float],
                    scores2: List[Float],
                    scores3: List[Float],
                    weightUnigrams: Float,
                    weightBigrams: Float,
                    weightTrigrams: Float): List[Float] = {
    def rescale(list: List[Float]): List[Float] = {
      val norm = list.sum
      list.map {
        case 0 => val x: Float = 0; x
        case x => x/norm
      }
    }
    (
      rescale(scores1).map(_ * weightUnigrams),
      rescale(scores2).map(_ * weightBigrams),
      rescale(scores3).map(_ * weightTrigrams)
    ).zipped.map(_ + _ + _)
  }

  /**
   * Computes, given a list of candidates and the context around the misspelled word, scores for each candidate.
   * For example
   *
   * scoreCandidates(["you", "lol"],
   *                 ["how", "are"],
   *                 ["doing", "today"])
   * -> Map(
   *      "you" -> 0.98,
   *      "lol" -> 0.02
   *    )
   *
   * @param candidates list of candidates
   * @param leftContext words preceding the misspelled word (at most two)
   * @param rightContext words following the misspelled word (at most two)
   * @param weightUnigrams weight for unigram in final score
   * @param weightBigrams weight for bigrams in final score
   * @param weightTrigrams weight for trigrams in final score
   * @return Map containing, for each candidate, the final score
   */
  def scoreCandidates(candidates: List[String],
                      leftContext: List[String],
                      rightContext: List[String],
                      weightUnigrams: Float,
                      weightBigrams: Float,
                      weightTrigrams: Float): Map[String, Float] = {
    // get stats from ES
    val (candidateCounts, ngramCounts) = getStats(candidates, leftContext, rightContext)
    def sumValues(mapObject: Map[String, Int], keys: List[String]): Int = {
      mapObject.filterKeys(keys.contains).values.sum
    }

    // create map with ngram scores for each candidate Map("cand1" -> (s11, s12, s13), "cand2" -> (s21, s22, s23), ...)
    val scoresNgrams = candidateCounts.map(x => (
      x._1,
      (
        x._2.getOrElse("t", 0).toFloat / ngramCounts.getOrElse("unigrams", 1).toFloat,
        sumValues(x._2, List("lt", "tr")).toFloat / ngramCounts.getOrElse("bigrams", 1).toFloat,
        sumValues(x._2, List("llt", "ltr", "trr")).toFloat / ngramCounts.getOrElse("trigrams", 1).toFloat
      )
    ))
    // combine scores
    // val testValues = List((0.1.toFloat,0.2.toFloat,0.3.toFloat), (0.11.toFloat,0.22.toFloat,0.33.toFloat))
    def reshapeListTuples3(x: (Float, Float, Float), l: (List[Float], List[Float], List[Float])) =
      (x._1 :: l._1, x._2 :: l._2, x._3 :: l._3)
    val scoresNgramsLists = scoresNgrams.values.foldRight[(List[Float], List[Float], List[Float])]((List(), List(), List()))(reshapeListTuples3)

    val scoresTotal = combineScores(
    // val scoresTotal = combineScoresAlternative(
      scoresNgramsLists._1,
      scoresNgramsLists._2,
      scoresNgramsLists._3,
      weightUnigrams,
      weightBigrams,
      weightTrigrams
    )
    scoresNgrams.keys.zip(scoresTotal).toMap
  }

  def suggestionsToToken(suggestions: List[SpellcheckToken2], index: Int): Token = {
    val token = Token(
      text = suggestions(index).text,
      offset = suggestions(index).offset,
      length = suggestions(index).length,
      isMisspelled = suggestions(index).options.nonEmpty,
      options = suggestions(index).options
    )
    token
  }

  def termsSuggester2(indexName: String, request: SpellcheckTermsRequest2) : SpellcheckTermsResponse2 = {
    val suggestions = getSuggestions(indexName, request)

    // transform suggestions into list of tokens
    val tokenList = List.range(0, suggestions.size).map(suggestionsToToken(suggestions, _))
    for((token, i) <- tokenList.zipWithIndex){
      val leftContext = tokenList.slice(i - 2, i)
      token.setLeftContext(leftContext)
      val rightContext = tokenList.slice(i + 1, i + 3)
      token.setRightContext(rightContext)
      token.updateContextSize()
      token.updateProperContextSize()
    }
    // compute candidates for every misspelling
    for(token <- tokenList){
      val scores = scoreCandidates(
        candidates = token.options.map(_.text),
        leftContext = token.getProperContextLeft.map(_.text),
        rightContext = token.getProperContextRight.map(_.text),
        1, 1, 1
      )
      val newOptions = scores.map(x => SpellcheckTokenSuggestions2(score = x._2, freq = -100.0, text = x._1)).toList
      token.setOptions(newOptions)
      // debug statements
      println(token.text)
      println(token.getProperContextLeft.map(_.text))
      println(token.getProperContextRight.map(_.text))
    }
    // put together the results
    val newSuggestions = tokenList.map(
      token => SpellcheckToken2(
        text = token.text,
        offset = token.offset,
        length = token.length,
        options = token.options)
    )
    SpellcheckTermsResponse2(tokens = newSuggestions)
  }
}

object Main2 extends App {
  val tokens = List("test", "prova", "foo", "bar")
  val index = "index_getjenny_english_0"
  val service = SpellcheckService2

  // check suggestions for a single word from es
  println("\nTesting termSuggester")
  val suggestRequest = SpellcheckTermsRequest2(text = "This is a test setnence")
  val suggestions = service.termsSuggester2(index, suggestRequest)
  println(suggestions)

  // given candidates and context, get scores
  println("\nTesting scoring function")
  val candidates = List("you", "lol")
  val leftContext = List("how", "are")
  val rightContext = List("doing", "today")
  val scores = service.scoreCandidates(candidates, leftContext, rightContext, 1, 1, 1)
  println(scores)

/*  // test Token class functions
  // tokens for left context
  val tokenL1 = SpellcheckToken("Hello", isMisspelled = false, List(), List(), List(), 0, 0)
  val tokenL2 = SpellcheckToken("ths", isMisspelled = true, List(), List(), List(), 0, 0)
  val tokenL3 = SpellcheckToken("is", isMisspelled = false, List(), List(), List(), 0, 0)
  // tokens for right context
  val tokenR1 = SpellcheckToken("stupid", isMisspelled = false, List(), List(), List(), 0, 0)
  val tokenR2 = SpellcheckToken("test", isMisspelled = false, List(), List(), List(), 0, 0)
  val tokenR3 = SpellcheckToken("setnence", isMisspelled = true, List(), List(), List(), 0, 0)
  val tokenTest = SpellcheckToken("This", isMisspelled = false, List(), List(tokenL1, tokenL2, tokenL3),
    List(tokenR1, tokenR2, tokenR3), 0, 0)
  val properRightContext = tokenTest.getProperContext(tokenTest.rightContext, List())
  val properLeftContext = tokenTest.getProperContext(tokenTest.leftContext.reverse, List()).reverse
  println(properLeftContext)*/

}
