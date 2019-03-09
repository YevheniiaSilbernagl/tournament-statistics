package controllers.discord

import java.awt.image.BufferedImage
import java.io.ByteArrayInputStream
import java.util.concurrent.TimeUnit

import akka.actor.ActorSystem
import controllers.{Battlefy, Cache}
import javax.imageio.ImageIO
import javax.inject.Inject
import play.api.Configuration
import play.api.libs.ws.WSClient
import play.api.mvc.Controller
import sx.blah.discord.api.{ClientBuilder, IDiscordClient}
import sx.blah.discord.handle.obj.{IGuild, IMessage, IPrivateChannel}

import scala.collection.JavaConversions._
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext}
import scala.language.implicitConversions
import scala.util.control.NonFatal

class Discord @Inject()(config: Configuration,
                        battlefy: Battlefy,
                        dropMe: DropMe,
                        resources: StreamingResources,
                        actorSystem: ActorSystem,
                        cache: Cache,
                        ws: WSClient)
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

  actorSystem.scheduler.schedule(initialDelay = 10.seconds, interval = 1.minute) {
    val bracketCacheKey = "currentBracket"
    battlefy.currentRound.foreach {
      round =>
        val tournament = battlefy.getCurrentTournament.copy(currentStage = battlefy.currentStageId)
        val currentBracket = tournament.bracketInfo
        val title = s"!title ${tournament.name} Round ${round._1} ${round._2.capitalize}"
        cache.get[String](bracketCacheKey) match {
          case Some(bracket) if bracket.equals(title) =>
          case _ =>
            talkToNightBot(s"!commands edit !bracket $currentBracket")
            talkToNightBot(title)
            cache.put(bracketCacheKey, title, 2 hours)
        }
    }
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

  def talkToNightBot(message: String): Unit = {
    bot.foreach(_.getChannels.toList
      .filter(_.getGuild.getName == "Eternal Tournament Series")
      .filter(_.getName == "tournament_chat").foreach {
      channel => channel.sendMessage(message)
    }
    )
  }

  def allPlayers: List[String] = bot.map(_.getGuilds.toList).getOrElse(List[IGuild]())
    .flatMap(guild => guild.getUsers.filterNot(_.isBot)
      .flatMap(user => List(
        user.getDisplayName(guild) + "#" + user.getDiscriminator,
        user.getName + "#" + user.getDiscriminator
      ))).distinct

  def getAvatar(name: String): Option[BufferedImage] = {
    bot.flatMap { b =>
      val strict = b.getUsersByName(name).headOption
      val similar = allPlayers.filter(_.contains(s"$name#"))
      if (strict.isEmpty && similar.length == 1) {
        b.getUsersByName(similar.head.split("#")(0)).headOption
      } else strict
    }.filter(_.getAvatar != null).map { user =>
      try {
        val avatarUrl = s"https://cdn.discordapp.com/avatars/${user.getStringID}/${user.getAvatar}.png"
        val response = Await.result(ws.url(avatarUrl).get(), Duration.apply(30, TimeUnit.SECONDS))
        ImageIO.read(new ByteArrayInputStream(response.bodyAsBytes))
      } catch {
        case NonFatal(e) => null
      }
    }
  }
}

