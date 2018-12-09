package controllers

import sx.blah.discord.api.events.EventSubscriber
import sx.blah.discord.handle.impl.events.guild.channel.message.reaction.ReactionAddEvent

import scala.collection.JavaConversions._

class CheckInCommandHandler {

  @EventSubscriber
  def onMessageReceived(event: ReactionAddEvent): Unit = {
    event.getMessage.getReactions.toList.find(_.getEmoji.getName == "\uD83D\uDC4C") match {
      case Some(_) =>
        event.getChannel.sendMessage(s"${event.getAuthor.mention()} thank you")
      case _ =>
    }
  }
}
