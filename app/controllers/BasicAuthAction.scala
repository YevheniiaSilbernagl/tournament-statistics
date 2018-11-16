package controllers

import org.apache.commons.codec.digest.DigestUtils
import play.api.mvc._

import scala.concurrent.Future

class BasicAuthAction(db: DB, battlefy: Battlefy) extends ActionBuilder[Request] with ActionFilter[Request] {

  private val unauthorized = Results.Unauthorized.withHeaders("WWW-Authenticate" -> "Basic realm=Unauthorized")

  def filter[A](request: Request[A]): Future[Option[Result]] = {
    val result = request.headers.get("Authorization") map { authHeader =>
      val (user, pass) = decodeBasicAuth(authHeader)
      db.pass(user) match {
        case Some(password) if password == DigestUtils.sha1Hex(pass)=> None
        case _ => Some(unauthorized)
      }
    } getOrElse Some(unauthorized)
    Future.successful(result)
  }

  private[this] def decodeBasicAuth(authHeader: String): (String, String) = {
    val baStr = authHeader.replaceFirst("Basic ", "")
    val decoded = new sun.misc.BASE64Decoder().decodeBuffer(baStr)
    val Array(user, password) = new String(decoded).split(":")
    (user, password)
  }
}