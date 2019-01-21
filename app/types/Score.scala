package types

case class Score(
                  current_player_id: Int,
                  participant_a_id: Int,
                  participant_b_id: Int,
                  participant_a_score: Int,
                  participant_b_score: Int,
                  round: Int, bracket_name: String
                ) {
  def isWinner: Boolean =
    if (current_player_id == participant_a_id) participant_a_score > participant_b_score
    else participant_b_score > participant_a_score
}
