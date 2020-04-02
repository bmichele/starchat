package com.getjenny.starchat.services

/**
 * Created by Angelo Leto <angelo@getjenny.com> on 01/12/17.
 */

import com.getjenny.starchat.entities.io.Permissions.Permission
import com.getjenny.starchat.entities.io.{RefreshPolicy, _}
import com.getjenny.starchat.services.auth.AbstractStarChatAuthenticator
import com.getjenny.starchat.services.esclient.UserElasticClient
import com.getjenny.starchat.services.esclient.crud.EsCrudBase
import com.getjenny.starchat.utils.Index
import com.typesafe.config.{Config, ConfigFactory}
import javax.naming.AuthenticationException
import org.elasticsearch.action.delete.DeleteResponse
import org.elasticsearch.action.get.GetResponse
import org.elasticsearch.action.index.IndexResponse
import org.elasticsearch.action.update.UpdateResponse
import org.elasticsearch.common.xcontent.XContentBuilder
import org.elasticsearch.common.xcontent.XContentFactory._
import org.elasticsearch.index.query.QueryBuilders
import org.elasticsearch.rest.RestStatus
import scalaz.Scalaz._

import scala.collection.JavaConverters._
import scala.collection.immutable.Stream
import scala.util.Random

case class UserEsServiceException(message: String = "", cause: Throwable = None.orNull)
  extends Exception(message, cause)

/**
 * Implements functions, eventually used by IndexManagementResource, for ES index management
 */
class UserEsService extends AbstractUserService {
  private[this] val config: Config = ConfigFactory.load()
  private[this] val elasticClient: UserElasticClient.type = UserElasticClient
  private[this] val indexName: String = Index.indexName(elasticClient.indexName, elasticClient.indexSuffix)
  private[this] val random: Random.type = scala.util.Random

  private[this] val admin: String = config.getString("starchat.basic_http_es.admin")
  private[this] val password: String = config.getString("starchat.basic_http_es.password")
  private[this] val salt: String = config.getString("starchat.basic_http_es.salt")
  private[this] val adminUser = User(id = admin, password = password, salt = salt,
    permissions = Map("admin" -> Set(Permissions.admin)))

  val esCrudBase = new EsCrudBase(elasticClient, indexName)

  def create(user: User): IndexDocumentResult = {

    if (user.id === "admin") {
      throw new AuthenticationException("admin user cannot be created via APIs")
    } else if (user.id.startsWith("index_")) {
      throw new AuthenticationException("Invalid pattern for username: \"index_\"")
    }

    val builder = createXContentBuilder(user.id, user.password.some, user.salt.some, user.permissions.some)
    val response: IndexResponse = esCrudBase.create(id = user.id, builder = builder,
      refreshPolicy = RefreshPolicy.`false`)

    val docResult: IndexDocumentResult = IndexDocumentResult(index = response.getIndex,
      id = response.getId,
      version = response.getVersion,
      created = response.status === RestStatus.CREATED
    )

    docResult
  }

  def update(user: UserUpdate): UpdateDocumentResult = {
    val builder = createXContentBuilder(user.id, user.password, user.salt, user.permissions)

    val response: UpdateResponse = esCrudBase.update(id = user.id, builder = builder,
      refreshPolicy = RefreshPolicy.`false`)

    UpdateDocumentResult(index = response.getIndex,
      id = response.getId,
      version = response.getVersion,
      created = response.status === RestStatus.CREATED
    )
  }

  private[this] def createXContentBuilder(id: String, password: Option[String], salt: Option[String],
                                          permissions: Option[Map[String, Set[Permission]]]): XContentBuilder = {
    if (id === "admin") {
      throw new AuthenticationException("admin user cannot be changed")
    }

    val builder: XContentBuilder = jsonBuilder().startObject()
    builder.field("id", id)

    password match {
      case Some(t) => builder.field("password", t)
      case None => ;
    }

    salt match {
      case Some(t) => builder.field("salt", t)
      case None => ;
    }

    permissions match {
      case Some(p) =>
        val permission = builder.startObject("permissions")
        p.foreach { case (permIndexName, userPermissions) =>
          val array = permission.field(permIndexName).startArray()
          userPermissions.foreach(p => {
            array.value(p.toString)
          }) // for each permission
          array.endArray()
        }
        permission.endObject()
      case None => ;
    }

    builder.endObject()
    builder
  }

  def delete(user: UserId): DeleteDocumentResult = {
    if (user.id === "admin") {
      throw new AuthenticationException("admin user cannot be changed")
    }

    val response: DeleteResponse = esCrudBase.delete(user.id, RefreshPolicy.`true`)

    DeleteDocumentResult(index = response.getIndex,
      id = response.getId,
      version = response.getVersion,
      found = response.status =/= RestStatus.NOT_FOUND
    )
  }

  def read(user: UserId): User = {
    if (user.id === "admin") {
      adminUser
    } else {
      val response: GetResponse = esCrudBase.read(user.id)

      val source = if (response.getSource != None.orNull) {
        response.getSource.asScala.toMap
      } else {
        throw UserEsServiceException("Cannot find user: " + id)
      }

      sourceToUser(source)
    }
  }

  private[this] def sourceToUser(source: Map[String, AnyRef]): User = {
    val userId: String = source.get("id") match {
      case Some(t) => t.asInstanceOf[String]
      case None => throw UserEsServiceException("User id field is empty for: " + id)
    }

    val password: String = source.get("password") match {
      case Some(t) => t.asInstanceOf[String]
      case None => throw UserEsServiceException("Password is empty for the user: " + id)
    }

    val salt: String = source.get("salt") match {
      case Some(t) => t.asInstanceOf[String]
      case None => throw UserEsServiceException("Salt is empty for the user: " + id)
    }

    val permissions: Map[String, Set[Permissions.Value]] = source.get("permissions") match {
      case Some(t) => t.asInstanceOf[java.util.HashMap[String, java.util.List[String]]]
        .asScala.map { case (permIndexName, userPermissions) =>
        (permIndexName, userPermissions.asScala.map(permissionString => Permissions.value(permissionString)).toSet)
      }.toMap
      case None =>
        throw UserEsServiceException("Permissions list is empty for the user: " + id)
    }
    User(id = userId, password = password, salt = salt, permissions = permissions)
  }

  /** given id and optionally password and permissions, generate a new user */
  def genUser(user: UserUpdate, authenticator: AbstractStarChatAuthenticator): User = {

    val passwordPlain = user.password match {
      case Some(t) => t
      case None =>
        generatePassword()
    }

    val salt = user.salt match {
      case Some(t) => t
      case None =>
        generateSalt()
    }

    val password = authenticator.hashedSecret(password = passwordPlain, salt = salt)

    val permissions = user.permissions match {
      case Some(t) => t
      case None =>
        Map.empty[String, Set[Permissions.Value]]
    }

    User(id = user.id, password = password, salt = salt, permissions = permissions)
  }

  private[this] val addDisabled = (p: Set[Permission]) => p + Permissions.disabled
  private[this] val removeDisabled = (p: Set[Permission]) => p - Permissions.disabled

  private[this] def updateUserList(list: List[User], index: String)(f: Set[Permission] => Set[Permission]): List[User] = {
    list
      .filter(user => user.permissions.contains(index))
      .map { user =>
        val updatedPermission = user.permissions.get(index) match {
          case Some(p) => val newPermission = f(p)
            user.permissions + (index -> newPermission)
          case None => user.permissions
        }
        user.copy(permissions = updatedPermission)
      }
  }

  def disablePermissionForIndex(index: String): List[User] = updatePermissions(index, addDisabled)

  def enablePermissionForIndex(index: String): List[User] = updatePermissions(index, removeDisabled)

  private[this] def updatePermissions(index: String, f: Set[Permission] => Set[Permission]): List[User] = {
    val allUsers = readAll()
    val updatedUsers = updateUserList(allUsers, index)(f)
    if(updatedUsers.nonEmpty){
      esCrudBase.bulkUpdate(
        elems = updatedUsers.map(u => u.id -> createXContentBuilder(u.id, None, None, u.permissions.some)),
        refreshPolicy = RefreshPolicy.`false`
      )
    }
    updatedUsers
  }

  def readAll(): List[User] = {
    val response = esCrudBase.read(QueryBuilders.matchAllQuery())
    response.getHits.getHits.toList.map(x => sourceToUser(x.getSourceAsMap.asScala.toMap))
  }

  private[this] def randomStrGenerator: Stream[Char] = {
    val chars: String = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789;:'`~-+_=[]{}\\|<,.>!@#$%^&*()"
    def nextAlphaNum: Char = {
      chars charAt random.nextInt(chars.length)
    }
    Stream continually nextAlphaNum
  }

  def generatePassword(size: Int = 32): String = {
    randomStrGenerator.take(size).mkString
  }

  def generateSalt(size: Int = 16): String = {
    randomStrGenerator.take(size).mkString
  }
}