package com.getjenny.starchat.resources

/**
 * Created by Michele Boggia <michele.boggia@getjenny.com> on 05/05/20.
 */

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.pattern.CircuitBreaker
import com.getjenny.starchat.entities.io.{Permissions, ReturnMessageData, SpellcheckTermsRequest2}
import com.getjenny.starchat.routing._
import com.getjenny.starchat.services.SpellcheckService2

import scala.util.{Failure, Success}

trait SpellcheckResource2 extends StarChatResource {

  private[this] val spellcheckService: SpellcheckService2.type = SpellcheckService2

  def spellcheckRoutes2: Route = handleExceptions(routesExceptionHandler) {
    pathPrefix(indexRegex ~ Slash ~ "spellcheck2") { indexName =>
      pathPrefix("terms") {
        pathEnd {
          post {
            authenticateBasicAsync(realm = authRealm,
              authenticator = authenticator.authenticator) { user =>
              authorizeAsync(_ =>
                authenticator.hasPermissions(user, indexName, Permissions.read)) {
                extractRequest { request =>
                  entity(as[SpellcheckTermsRequest2]) { spellCheckRequest =>
                    val breaker: CircuitBreaker = StarChatCircuitBreaker.getCircuitBreaker()
                    onCompleteWithBreakerFuture(breaker)(spellcheckService.termsSuggester2(indexName, spellCheckRequest)) {
                      case Success(t) =>
                        completeResponse(StatusCodes.OK, StatusCodes.BadRequest, t)
                      case Failure(e) =>
                        log.error(logTemplate(user.id, indexName, "spellcheckRoutes2", request.method, request.uri), e)
                        completeResponse(StatusCodes.BadRequest,
                          Option {
                            ReturnMessageData(code = 100, message = e.getMessage)
                          })
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}
