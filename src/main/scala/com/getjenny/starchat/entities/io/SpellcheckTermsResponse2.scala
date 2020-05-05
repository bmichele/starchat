package com.getjenny.starchat.entities.io

/**
 * Created by angelo on 21/04/17.
 */

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
