package controllers.discord

import javax.inject.Inject
import sx.blah.discord.api.events.EventSubscriber
import sx.blah.discord.handle.impl.events.guild.channel.message.MessageReceivedEvent

case class ResourcesCommandHandler @Inject()(resources: StreamingResources) {

  @EventSubscriber
  def onMessageReceived(event: MessageReceivedEvent): Unit = {
    def privateMessageToBot: Boolean = event.getChannel.getName == event.getAuthor.getName

    if (privateMessageToBot) {
      event.getMessage match {
        case message if resources.isSubscribeStreamer(message) =>
          resources.subscribeOnBehalfOfStreamer(message)

        case message if resources.isSubscribeCommand(message) =>
          resources.subscribeStreamer(event.getAuthor)

        case message if resources.isHelpMeComand(message) =>
          resources.help(event)

        case _ =>
      }
    }
  }
}
