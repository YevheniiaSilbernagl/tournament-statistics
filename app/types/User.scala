package types

import org.apache.commons.codec.digest.DigestUtils.sha1Hex

case class User(login: String,
                password: Option[String],
                role: Option[String]){
  def passwordHash: Option[String] = password.map(sha1Hex)
}