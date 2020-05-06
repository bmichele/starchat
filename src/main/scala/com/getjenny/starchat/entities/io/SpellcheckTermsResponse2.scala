package com.getjenny.starchat.entities.io

/**
 * Created by Michele Boggia <michele.boggia@getjenny.com> on 05/05/20.
 */
// TODO: this class definitions are useful only for debugging purpose, so that I can show ngram scores together with
//  final score. In case we don't want to show all these details, use classes defined in SpellCheckTermsResponse.scala
case class SpellcheckTokenSuggestions2(
                                       score: Double,
                                       freq: Double,
                                       text: String
                                     )

case class SpellcheckToken2(
                            text: String,
                            offset: Int,
                            length: Int,
                            options: List[SpellcheckTokenSuggestions2]
                          )

case class SpellcheckTermsResponse2(
                                    tokens: List[SpellcheckToken2]
                                  )
