package controllers.discord

import controllers.Cache
import javax.inject.Inject
import org.joda.time.DateTime
import play.api.mvc.Controller
import sx.blah.discord.api.IDiscordClient
import sx.blah.discord.handle.impl.events.guild.channel.message.MessageReceivedEvent
import sx.blah.discord.handle.obj.{IMessage, IPrivateChannel, IUser}

import scala.collection.JavaConversions._
import scala.concurrent.duration._
import scala.language.implicitConversions

class StreamingResources @Inject()(cache: Cache) extends Controller {
  type Channel = String
  type Name = String
  type Drop = (Long, Name, Channel, DateTime)

  val subscriptionCacheKey = "resources_subscribed"
  val cacheDuration: FiniteDuration = 1.day

  val resourceCommand = "!resource"

  val messageForSubscribedStreamer = s"You subscribed for streaming resources. Subscription expires in $cacheDuration"
  val subscribeCommand = s"$resourceCommand subscribe"
  val helpCommand = s"$resourceCommand help"

  val instructions: String =
    s"""
       |**$subscribeCommand** - request to receive generated streaming resources
     """.stripMargin

  def isSubscribeCommand(message: IMessage): Boolean = message.getContent == subscribeCommand

  def isSubscribeStreamer(message: IMessage): Boolean = {
    val content = message.getContent
    val name = content.replace(subscribeCommand, "").trim
    getUser(message.getClient, name).isDefined
  }

  def isHelpMeComand(message: IMessage): Boolean = message.getContent == helpCommand

  def subscribeStreamer(user: IUser): Unit = {
    val currentlySubscribed = cache.get[List[Long]](subscriptionCacheKey).getOrElse(List())
    cache.put(subscriptionCacheKey, (currentlySubscribed :+ user.getLongID).distinct, cacheDuration)
    user.getOrCreatePMChannel().sendMessage(messageForSubscribedStreamer)
  }

  def subscribeOnBehalfOfStreamer(message: IMessage): Unit = {
    val name = message.getContent.replace(subscribeCommand, "").trim
    getUser(message.getClient, name).foreach(subscribeStreamer)
    message.getAuthor.getOrCreatePMChannel().sendMessage(s"$name has been successfully subscribed")
  }

  def help(event: MessageReceivedEvent): IMessage = {
    event.getAuthor.getOrCreatePMChannel().sendMessage(instructions)
  }

  def getUser(client: IDiscordClient, name: String): Option[IUser] = {
    client.getChannels.toList.flatMap(channel => channel.getUsersHere.toList)
      .filter(user => name == (user.getName + "#" + user.getDiscriminator)).distinct.headOption
  }

  def listOfStreamers: List[Long] = cache.get[List[Long]](subscriptionCacheKey).getOrElse(List())

  def notifyStreamers(discord: IDiscordClient, operation: IPrivateChannel => IMessage): Unit = {
    listOfStreamers.distinct.foreach(streamer => operation(discord.getUserByID(streamer).getOrCreatePMChannel()))
  }
}
