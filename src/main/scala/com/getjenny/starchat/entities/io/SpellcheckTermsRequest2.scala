package com.getjenny.starchat.entities.io

/**
 * Created by michele.boggia@getjenny.com on 05/05/20.
 */

case class SpellcheckTermsRequest2(
                                   text: String,
                                   prefixLength: Int = 0,
                                   minDocFreq: Int = 1,
                                   minWordLength: Int = 2,
                                   maxEdit: Int = 2
                                 )
