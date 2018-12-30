package controllers

import controllers.Battlefy._
import javax.inject.Inject
import sx.blah.discord.api.events.EventSubscriber
import sx.blah.discord.handle.impl.events.guild.channel.message.MessageReceivedEvent

case class DropMeCommandHandler @Inject()(battlefy: Battlefy, dropMe: DropMe) {
  val allowedChannels = List("bot-testing", "tournament-chat", "tournament-help")

  @EventSubscriber
  def onMessageReceived(event: MessageReceivedEvent): Unit = {
    def privateMessageToBot: Boolean = event.getChannel.getName == event.getAuthor.getName

    if (privateMessageToBot) {
      event.getMessage match {
        case message if dropMe.isSubscribeCommand(message) =>
          dropMe.subscribeTO(event.getAuthor)

        case message if event.getAuthor.isParticipant(battlefy) && (dropMe.isConfirmationToDrop(message) || dropMe.isRequestToDrop(message)) =>
          dropMe.confirmToDrop(event)

        case message if dropMe.isCancelCommand(message) && event.getAuthor.isParticipant(battlefy) =>
          dropMe.cancel(event)

        case _ =>
          dropMe.help(event)
      }
    } else if (allowedChannels.contains(event.getChannel.getName) || privateMessageToBot) {
      event.getMessage match {
        case message if dropMe.isDropMeMessage(message) && event.getAuthor.isParticipant(battlefy) =>
          dropMe.scheduleToDrop(event)

        case _ =>
      }
    }
  }
}
