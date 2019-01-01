package controllers

import akka.actor.ActorSystem
import javax.inject.Inject
import play.api.Configuration
import play.api.mvc.Controller
import sx.blah.discord.api.{ClientBuilder, IDiscordClient}

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.language.implicitConversions

class Discord @Inject()(config: Configuration, battlefy: Battlefy, dropMe: DropMe, actorSystem: ActorSystem, cache: Cache)
                       (implicit executionContext: ExecutionContext) extends Controller {
  val bot: Option[IDiscordClient] = config.getString("discord.bot.token").map { token =>
    val client = new ClientBuilder()
      .withToken(token)
      .withRecommendedShardCount()
      .build()
    client.getDispatcher.registerListener(CheckInCommandHandler(battlefy))
    client.getDispatcher.registerListener(DropMeCommandHandler(battlefy, dropMe))
    client.login()
    client
  }

  actorSystem.scheduler.schedule(initialDelay = 10.seconds, interval = 1.minute) {
    bot.foreach(discordBot => dropMe.notifyTO(discordBot))
  }
}

