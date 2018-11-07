package types

case class Score(
                  current_player_id: Int,
                  participant_a_id: Int,
                  participant_b_id: Int,
                  participant_a_score: Int,
                  participant_b_score: Int,
                  round: Int, bracket_name: String
           )
