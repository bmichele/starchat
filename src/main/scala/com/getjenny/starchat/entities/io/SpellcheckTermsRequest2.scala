package com.getjenny.starchat.entities.io

/**
 * Created by Michele Boggia <michele.boggia@getjenny.com> on 05/05/20.
 */

case class SpellcheckTermsRequest2(
                                   text: String,
                                   prefixLength: Int = 3,
                                   minDocFreq: Int = 1,
                                   maxEdit: Int = 2
                                 )
