package controllers.discord

import akka.actor.ActorSystem
import controllers.{Battlefy, Cache}
import javax.inject.Inject
import play.api.Configuration
import play.api.mvc.Controller
import sx.blah.discord.api.{ClientBuilder, IDiscordClient}
import sx.blah.discord.handle.obj.{IGuild, IMessage, IPrivateChannel}

import scala.collection.JavaConversions._
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.language.implicitConversions

class Discord @Inject()(config: Configuration,
                        battlefy: Battlefy,
                        dropMe: DropMe,
                        resources: StreamingResources,
                        actorSystem: ActorSystem,
                        cache: Cache)
                       (implicit executionContext: ExecutionContext) extends Controller {
  private val bot: Option[IDiscordClient] = config.getString("discord.bot.token").map { token =>
    val client = new ClientBuilder()
      .withToken(token)
      .withRecommendedShardCount()
      .build()
    client.getDispatcher.registerListener(CheckInCommandHandler(battlefy))
    client.getDispatcher.registerListener(DropMeCommandHandler(battlefy, dropMe))
    client.getDispatcher.registerListener(ResourcesCommandHandler(resources))
    client.login()
    client
  }

  actorSystem.scheduler.schedule(initialDelay = 10.seconds, interval = 1.minute) {
    bot.foreach(discordBot => dropMe.notifyTO(discordBot))
  }

  def notifyStreamers(operation: IPrivateChannel => IMessage): Unit = {
    bot.foreach(client => resources.notifyStreamers(client, operation))
  }

  def notifyAdmin(operation: IPrivateChannel => IMessage): Unit = {
    bot.foreach(client => operation(client.getApplicationOwner.getOrCreatePMChannel()))
  }

  def sendPM(user: String, message: String): Option[IMessage] = {
    bot.flatMap { channel =>
      channel.getUsers.find(_.getName == user).map(u => u.getOrCreatePMChannel().sendMessage(message))
    }
  }

  def allPlayers: List[String] = bot.map(_.getGuilds.toList).getOrElse(List[IGuild]())
    .flatMap(guild => guild.getUsers.filterNot(_.isBot)
      .flatMap(user => List(
        user.getDisplayName(guild) + "#" + user.getDiscriminator,
        user.getName + "#" + user.getDiscriminator
      ))).distinct
}

