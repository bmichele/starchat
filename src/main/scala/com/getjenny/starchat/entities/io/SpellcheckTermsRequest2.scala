package com.getjenny.starchat.entities.io

import org.elasticsearch.search.suggest.term.TermSuggestionBuilder.{SuggestMode, StringDistanceImpl}

/**
 * Created by michele.boggia@getjenny.com on 05/05/20.
 */

case class SpellcheckTermsRequest2(
                                   text: String,
                                   prefixLength: Int = 0,
                                   minDocFreq: Int = 1,
                                   minWordLength: Int = 2,
                                   maxEdit: Int = 2,
                                   suggestMode: SuggestMode = SuggestMode.MISSING,
                                   stringDistance: StringDistanceImpl = StringDistanceImpl.DAMERAU_LEVENSHTEIN,
                                   size: Int = 100
                                 )
