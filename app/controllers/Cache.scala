package controllers

import java.net.InetSocketAddress

import javax.inject.Inject
import net.rubyeye.xmemcached.auth.AuthInfo
import net.rubyeye.xmemcached.command.BinaryCommandFactory
import net.rubyeye.xmemcached.utils.AddrUtil
import net.rubyeye.xmemcached.{MemcachedClient, MemcachedClientBuilder, XMemcachedClientBuilder}
import play.api.Configuration
import play.api.mvc.Controller

import scala.collection.JavaConversions._
import scala.concurrent.duration._
import scala.util.control.NonFatal

class Cache @Inject()(config: Configuration) extends Controller {

  private val mc: MemcachedClient =
    (config.getString("memcached.servers"), config.getString("memcached.username"), config.getString("memcached.password")) match {
      case (Some(serverAddresses), Some(username), Some(password)) =>
        val servers: List[InetSocketAddress] = AddrUtil.getAddresses(serverAddresses.replace(",", " ")).toList
        val authInfo: AuthInfo = AuthInfo.plain(username, password)
        val builder: MemcachedClientBuilder = new XMemcachedClientBuilder(servers)
        servers.foreach(server => {
          builder.addAuthInfo(server, authInfo)
        })
        builder.setCommandFactory(new BinaryCommandFactory())
        builder.setConnectTimeout(1000)
        builder.setEnableHealSession(true)
        builder.setHealSessionInterval(2000)
        builder.build()
      case _ => throw new RuntimeException("No configuration for Memcachier")
    }

  def get[T](key: String): Option[T] =
    try {
      Option(mc.get[T](normalize(key)))
    }
    catch {
      case NonFatal(e) => None
    }

  def normalize(key: String): String = key.replaceAll("\\/:", "_")

  def put(key: String, value: AnyRef, duration: FiniteDuration): Unit = {
    mc.set(normalize(key), duration.toSeconds.intValue(), value)
  }

  def delete(key: String): Unit = {
    mc.delete(key)
  }

  def invalidate(): Unit = {
    mc.flushAll()
  }
}
