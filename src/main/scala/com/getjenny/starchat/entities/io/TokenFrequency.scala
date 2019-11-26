package com.getjenny.starchat.entities.io

case class TokenFrequency(
                           tokensFreq: List[TokenFrequencyItem],
                           priorTotalTerms: Long,
                           observedTotalTerms: Long
                         )

case class TokenFrequencyItem(
                               token: String,
                               priorFrequency: Double,
                               observedFrequency: Double
                             )
