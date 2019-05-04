package controllers.discord

import java.util.concurrent.TimeUnit

import controllers.Cache
import javax.inject.Inject
import org.joda.time.DateTime
import play.api.mvc.Controller
import sx.blah.discord.api.IDiscordClient
import sx.blah.discord.handle.impl.events.guild.channel.message.MessageReceivedEvent
import sx.blah.discord.handle.obj.{IMessage, IUser}

import scala.collection.JavaConversions._
import scala.concurrent.duration._
import scala.language.implicitConversions

class DropMe @Inject()(cache: Cache) extends Controller {
  type Channel = String
  type Name = String
  type Drop = (Long, Name, Channel, DateTime)

  val subscriptionCacheKey = "drops_subscribed"
  val dropsCacheKey = "drops"
  val cacheDuration: FiniteDuration = 3.days
  val delayBeforeNotificationToTO: FiniteDuration = 5.minutes

  val dropMeCommand = "!dropme"

  val allowedWords = List("drop", "dropping", "!drop")

  val subscribeCommand = s"$dropMeCommand subscribe"
  val dropMeConfirmCommand = s"$dropMeCommand confirm"
  val dropMeRequestCommand = s"$dropMeCommand request"
  val dropMeCancelCommand = s"$dropMeCommand cancel"
  val dropMeHelpCommand = s"$dropMeCommand help"

  val messageForSubscribedAdmin = s"You subscribed for $dropMeCommand messages. Subscription expires in $cacheDuration"

  val confirmConfirmation = s"TO's have been notified about your request"

  val cancelationConfirmed = s"Drop request cancelled"

  val messageToUserRequestConfirmation: String =
    s"""If you want to drop the tournament please type following command in this chat:
       |**$dropMeConfirmCommand**
       |
       |If you don't want to drop please type:
       |**$dropMeCancelCommand**
     """.stripMargin

  val instructions: String =
    s"""
       |**$dropMeRequestCommand** - request to drop from the tournament
       |**$dropMeConfirmCommand** - confirmation that you want to drop from the tournament
       |**$dropMeCancelCommand** - cancel the current drop request
     """.stripMargin

  def userConfirmed(user: IUser) = s"""**${user.getName}** confirmed that they want to drop"""

  def potentialDrop(discord: IDiscordClient, drop: Drop): String =
    s"""User potentially wants to drop but hasn't confirmed:
       |>${drop._3} #${discord.getUserByID(drop._1).getName} - ${drop._2}""".stripMargin

  def isSubscribePerson(message: IMessage): Boolean = {
    val content = message.getContent
    val name = content.replace(subscribeCommand, "").trim
    getUser(message.getClient, name).isDefined
  }

  def isSubscribeCommand(message: IMessage): Boolean = message.getContent == subscribeCommand

  def isDropMeMessage(message: IMessage): Boolean = {
    allowedWords.exists(word => message.getContent.toLowerCase.trim.split("\\s").contains(word))
  }

  def isConfirmationToDrop(message: IMessage): Boolean = message.getContent == dropMeConfirmCommand

  def isRequestToDrop(message: IMessage): Boolean = message.getContent == dropMeRequestCommand

  def isCancelCommand(message: IMessage): Boolean = message.getContent == dropMeCancelCommand

  def isHelpMeComand(message: IMessage): Boolean = message.getContent == dropMeHelpCommand

  def subscribeTO(user: IUser): Unit = {
    val currentlySubscribed = cache.get[List[Long]](subscriptionCacheKey).getOrElse(List())
    cache.put(subscriptionCacheKey, (currentlySubscribed :+ user.getLongID).distinct, cacheDuration)
    user.getOrCreatePMChannel().sendMessage(messageForSubscribedAdmin)
  }

  def getUser(client: IDiscordClient, name: String): Option[IUser] = {
    client.getChannels.toList.flatMap(channel => channel.getUsersHere.toList)
      .filter(user => name == (user.getName + "#" + user.getDiscriminator)).distinct.headOption
  }

  def subscribeOnBehalfOfTO(message: IMessage): Unit = {
    val name = message.getContent.replace(subscribeCommand, "").trim
    getUser(message.getClient, name).foreach(subscribeTO)
    message.getAuthor.getOrCreatePMChannel().sendMessage(s"$name has been successfully subscribed")
  }

  def scheduleToDrop(event: MessageReceivedEvent): Unit = {
    val drops: List[Drop] = cache.get[List[Drop]](dropsCacheKey).getOrElse(List())
    val newDrop = (event.getAuthor.getLongID, event.getMessage.getContent, event.getMessage.getChannel.getName, DateTime.now)
    cache.put(dropsCacheKey, drops :+ newDrop, cacheDuration)
    event.getAuthor.getOrCreatePMChannel().sendMessage(messageToUserRequestConfirmation)
  }

  def confirmToDrop(event: MessageReceivedEvent): Unit = {
    listOfTOs.foreach { to =>
      event.getClient.getUserByID(to).getOrCreatePMChannel().sendMessage(userConfirmed(event.getAuthor))
    }
    val drops: List[Drop] = cache.get[List[Drop]](dropsCacheKey).getOrElse(List())
    cache.put(dropsCacheKey, drops.filterNot(m => m._1 == event.getAuthor.getLongID), cacheDuration)
    event.getAuthor.getOrCreatePMChannel().sendMessage(confirmConfirmation)
  }

  def cancel(event: MessageReceivedEvent): Unit = {
    val drops: List[Drop] = cache.get[List[Drop]](dropsCacheKey).getOrElse(List())
    cache.put(dropsCacheKey, drops.filterNot(m => m._1 == event.getAuthor.getLongID), cacheDuration)
    event.getAuthor.getOrCreatePMChannel().sendMessage(cancelationConfirmed)
  }

  def help(event: MessageReceivedEvent): Unit = {
    event.getAuthor.getOrCreatePMChannel().sendMessage(instructions)
  }

  def listOfTOs: List[Long] = cache.get[List[Long]](subscriptionCacheKey).getOrElse(List())

  def notifyTO(discord: IDiscordClient): Unit = {
    val drops = cache.get[List[Drop]](dropsCacheKey).getOrElse(List())
    val dropsToProcess = drops.filter(dropMessage => dropMessage._4.isBefore(DateTime.now.minusMinutes(delayBeforeNotificationToTO.toUnit(TimeUnit.MINUTES).intValue())))
    dropsToProcess.foreach { m =>
      listOfTOs.distinct.foreach(to => discord.getUserByID(to).getOrCreatePMChannel().sendMessage(potentialDrop(discord, m)))
    }
    cache.put(dropsCacheKey, drops.filterNot(dropsToProcess.contains(_)), cacheDuration)
  }
}
