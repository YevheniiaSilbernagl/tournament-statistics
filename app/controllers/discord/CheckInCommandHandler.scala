package controllers.discord

import controllers.Battlefy
import controllers.Battlefy._
import sx.blah.discord.api.events.EventSubscriber
import sx.blah.discord.handle.impl.events.guild.channel.message.MessageReceivedEvent
import sx.blah.discord.handle.impl.events.guild.channel.message.reaction.ReactionAddEvent
import sx.blah.discord.handle.obj.{IReaction, IUser}

import scala.collection.JavaConversions._

case class CheckInCommandHandler(battlefy: Battlefy) {
  private val XENORATH_EMOJI = "\uD83D\uDC4C"
  private val CHECK_IN_EMOJI = "✅"
  private val MESSAGE_TO_NOT_REGISTERED_PLAYER =
    s""""It appears you have tried to check in for an ETS event. Either you are not registered or you have registered with an incorrect discord name.
       |If you have registered contact the Tournament Organizer immediately""""

  def message_to_TO_about_not_registered_player(playerName: String): String = s"$playerName has attempted to check in. Their username was not found for the current tournament"

  val CHECK_IN_MESSAGE = s"To check in for today's ETS event respond to this comment with $CHECK_IN_EMOJI"
  val MESSAGE_FOR_WRONG_EMOJI = s"The emoji you have used to check in is not valid please use $CHECK_IN_EMOJI"
  val CHECK_IN_STARTED_MESSAGE = s"Check in started"

  @EventSubscriber
  def onMessageReceived(event: MessageReceivedEvent): Unit = {
    event.getMessage.getContent match {
      case message if message == CHECK_IN_MESSAGE => event.getAuthor.getOrCreatePMChannel().sendMessage(CHECK_IN_STARTED_MESSAGE)
      case _ =>
    }
  }

  @EventSubscriber
  def onMessageReceived(event: ReactionAddEvent): Unit = {

    def is_ok_hand(reaction: IReaction): Boolean = reaction.getEmoji.getName.startsWith(XENORATH_EMOJI)

    def is_check_in_emoji(reaction: IReaction): Boolean = reaction.getEmoji.getName.startsWith(CHECK_IN_EMOJI)

    def get_check_in_emoji: Option[IReaction] = event.getMessage.getReactions.toList.find(is_check_in_emoji)

    def check_in_closed: Boolean = event.getMessage.getReactions.toList.filter(reaction =>
      is_ok_hand(reaction)).exists(reaction => reaction.getUsers.contains(event.getAuthor))

    if (event.getMessage.getContent.startsWith(CHECK_IN_MESSAGE))
      event.getReaction match {
        case reaction if !check_in_closed && is_check_in_emoji(reaction) && !event.getUser.isParticipant(battlefy) =>
          event.getUser.getOrCreatePMChannel().sendMessage(MESSAGE_TO_NOT_REGISTERED_PLAYER)
          event.getMessage.getAuthor.getOrCreatePMChannel().sendMessage(message_to_TO_about_not_registered_player(event.getUser.getName))

        case reaction if !check_in_closed && is_check_in_emoji(reaction) =>

        case reaction if is_ok_hand(reaction) && event.getAuthor == event.getUser =>
          val checkIns: List[IUser] = get_check_in_emoji.map(_.getUsers.toList).getOrElse(List[IUser]())
          val players = battlefy.listOfPlayers(battlefy.getCurrentTournament.battlefy_id)
          val checkedInPlayers = players.filter(player => player._3.isDefined && checkIns.map(ci => ci.getName + "#" + ci.getDiscriminator).contains(player._3.get)).map(_._4)
          if (checkedInPlayers.nonEmpty) {
            event.getMessage.getAuthor.getOrCreatePMChannel().sendMessage(checkedInPlayers.mkString("\n"))
          }

        case _ if !check_in_closed =>
          val alreadyCheckedIn = get_check_in_emoji.exists { validEmoji =>
            validEmoji.getUsers.contains(event.getUser)
          }
          if (!alreadyCheckedIn) {
            if (event.getUser.isParticipant(battlefy)) {
              event.getUser.getOrCreatePMChannel().sendMessage(MESSAGE_FOR_WRONG_EMOJI)
            }
          }

        case _ =>
      }
  }
}
