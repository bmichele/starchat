package com.getjenny.starchat.entities.io

/**
 * Created by Angelo Leto <angelo@getjenny.com> on 01/07/16.
 */

case class DTDocumentSearch(from: Option[Int],
                            size: Option[Int],
                            executionOrder: Option[Int],
                            minScore: Option[Float],
                            boostExactMatchFactor: Option[Float],
                            state: Option[String],
                            evaluationClass: Option[String],
                            queries: Option[String],
                            searchAlgorithm: Option[SearchAlgorithm.Value],
                            timestampGte: Option[Long] = None, /* min indexing timestamp, None means no lower bound */
                            timestampLte: Option[Long] = None, /* max indexing timestamp, None means no upper bound*/
                            status: Option[DTDocumentStatus.Value] = None
                           )
