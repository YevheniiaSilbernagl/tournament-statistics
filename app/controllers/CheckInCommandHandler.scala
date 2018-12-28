package controllers

import sx.blah.discord.api.events.EventSubscriber
import sx.blah.discord.handle.impl.events.guild.channel.message.reaction.ReactionAddEvent
import sx.blah.discord.handle.obj.IUser

import scala.collection.JavaConversions._

case class CheckInCommandHandler(battlefy: Battlefy) {
  private val XENORATH_EMOJI = "\uD83D\uDC4C"
  private val CHECK_IN_EMOJI = "✅"
  private val MESSAGE_TO_NOT_REGISTERED_PLAYER = "It appears you have tried to check in for an ETS event that you have not registered for. If you have registered  contact the Tournament Organizer immediately"

  def message_to_TO_about_not_registered_player(playerName: String): String = s"$playerName has attempted to check in. Their username was not found for the current tournament"

  val CHECK_IN_MESSAGE = s"To check in for today's ETS event respond to this comment with $CHECK_IN_EMOJI"
  val MESSAGE_FOR_WRONG_EMOJI = s"The emoji you have used to check in is not valid please use $CHECK_IN_EMOJI"

  @EventSubscriber
  def onMessageReceived(event: ReactionAddEvent): Unit = {
    if (event.getMessage.getContent.startsWith(CHECK_IN_MESSAGE))
      event.getReaction match {
        case reaction if reaction.getEmoji.getName.startsWith(CHECK_IN_EMOJI) =>
          val players = battlefy.listOfPlayers(battlefy.getCurrentTournament.battlefy_id)
          val reactionAuthor = event.getUser
          val potentialPlayers = players.filter(player => player._3.isDefined && player._3.get.startsWith(s"${reactionAuthor.getName}#"))
          if (potentialPlayers.isEmpty) {
            reactionAuthor.getOrCreatePMChannel().sendMessage(MESSAGE_TO_NOT_REGISTERED_PLAYER)
            event.getMessage.getAuthor.getOrCreatePMChannel().sendMessage(message_to_TO_about_not_registered_player(reactionAuthor.getName))
          }
        case reaction if reaction.getEmoji.getName.startsWith(XENORATH_EMOJI) && event.getAuthor == event.getMessage.getAuthor =>
          val checkIns: List[IUser] = event.getMessage.getReactions.toList.find(reaction => reaction.getEmoji.getName.startsWith(CHECK_IN_EMOJI)).map(_.getUsers.toList).getOrElse(List[IUser]())
          val players = battlefy.listOfPlayers(battlefy.getCurrentTournament.battlefy_id)
          val checkedInPlayers = checkIns.flatMap(user => players.find(player => player._3.isDefined && player._3.get.startsWith(s"${user.getName}#")).map(_._4))
          if (checkedInPlayers.nonEmpty) {
            event.getMessage.getAuthor.getOrCreatePMChannel().sendMessage(checkedInPlayers.mkString("\n"))
          }
        case reaction =>
          val alreadyCheckedIn = event.getMessage.getReactions.toList.find { reaction =>
            reaction.getEmoji.getName.startsWith(CHECK_IN_EMOJI)
          }.exists { validEmoji =>
            validEmoji.getUsers.contains(event.getAuthor)
          }
          if (!alreadyCheckedIn) {
            val players = battlefy.listOfPlayers(battlefy.getCurrentTournament.battlefy_id)
            if (players.exists(player => player._3.isDefined && player._3.get.startsWith(s"${event.getAuthor.getName}#"))) {
              event.getAuthor.getOrCreatePMChannel().sendMessage(MESSAGE_FOR_WRONG_EMOJI)
            }
          }

      }
  }
}
