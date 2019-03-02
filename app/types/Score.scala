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
case class Score_(
                  current_player_name: String,
                  participant_a_name: String,
                  participant_b_name: String,
                  participant_a_score: Int,
                  participant_b_score: Int,
                  round: Int, bracket_name: String
                ) {
  def isWinner: Boolean =
    if (current_player_name == participant_a_name) participant_a_score > participant_b_score
    else participant_b_score > participant_a_score
}
