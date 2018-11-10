package controllers

import java.sql.Connection

import javax.inject.Inject
import org.joda.time.DateTime
import play.api.db.Database
import play.api.mvc.Controller
import types.{Score, Tournament}

import scala.collection.mutable

class DB @Inject()(database: Database) extends Controller {
  def opponentPreviousInteraction(playerName: String, opponent: String): List[(String, String, String)] = {
    val query =
      s"""
         |WITH games AS (SELECT *
         |               FROM match m
         |                 JOIN participant p ON (m.participant_a_id = p.id OR m.participant_b_id = p.id)
         |                 JOIN player p2 ON p.player_id = p2.id
         |               WHERE p2.eternal_name = '$playerName'),
         |    opponent_id AS (SELECT p4.id AS opponent_id
         |                    FROM participant p4
         |                      JOIN player p3 ON p4.player_id = p3.id
         |                    WHERE p3.eternal_name = '$opponent'),
         |    tournaments AS (SELECT *
         |                    FROM tournament)
         |SELECT
         |  g.*,
         |  opponent_id,
         |  t.name
         |FROM games g, opponent_id, tournaments t
         |WHERE (g.participant_a_id = opponent_id OR g.participant_b_id = opponent_id) AND t.id = g.tournament_id
       """.stripMargin
    val info: mutable.ListBuffer[(String, String, String)] = new mutable.ListBuffer[(String, String, String)]()
    val conn: Connection = database.getConnection()
    try {
      val stmt = conn.createStatement
      val rs = stmt.executeQuery(query)
      while (rs.next()) {
        val participant_a_score = rs.getInt("participant_a_score")
        val participant_b_score = rs.getInt("participant_b_score")
        val participant_a_id = rs.getInt("participant_a_id")
        val opponent_id = rs.getInt("opponent_id")
        val tournament_name = rs.getString("name")
        val round = rs.getInt("round")
        val bracket = rs.getString("bracket_name")
        info.+=((s"[$round $bracket] $tournament_name", s"$playerName - ${if (opponent_id == participant_a_id) participant_b_score else participant_a_score}", s"${if (opponent_id == participant_a_id) participant_a_score else participant_b_score} - $opponent"))
      }
    } finally {
      conn.close()
    }
    info.toList
  }


  implicit def dateTimeOrdering: Ordering[DateTime] = Ordering.fromLessThan(_ isBefore _)

  val eliminationRoundsCache: mutable.HashMap[String, Int] = scala.collection.mutable.HashMap()

  lazy val current_season: Int = {
    //if app is always online than wee need an update here
    val conn: Connection = database.getConnection()
    try {
      val stmt = conn.createStatement
      val rs = stmt.executeQuery("SELECT Max(season) as season FROM tournament t")
      rs.next()
      rs.getInt("season")
    } finally {
      conn.close()
    }
  }


  def getPlayers: Map[Int, String] = {
    val mutableListOfPlayers: mutable.HashMap[Int, String] = new mutable.HashMap[Int, String]()
    val conn: Connection = database.getConnection()
    try {
      val stmt = conn.createStatement
      val rs = stmt.executeQuery("SELECT * FROM player WHERE eternal_name != 'BYE+0000'")
      while (rs.next()) {
        mutableListOfPlayers.put(rs.getInt("id"), rs.getString("eternal_name"))
      }
    } finally {
      conn.close()
    }
    mutableListOfPlayers.toMap
  }


  def playerStats(playerId: Int): (String, List[(String, String, String)], Boolean) = {
    val tournaments: mutable.MutableList[(Tournament, Score, String)] = new mutable.MutableList[(Tournament, Score, String)]()
    val conn: Connection = database.getConnection()
    var name: String = ""
    try {
      val stmt = conn.createStatement
      val rs = stmt.executeQuery(
        s"""SELECT
           |  p.id                 AS player_id,
           |  p.eternal_name       AS player_name,
           |  part.id              AS participant_id,
           |  t.name               AS tournament_name,
           |  t.date               AS tournament_date,
           |  t.id                 AS tournament_id,
           |  t.battlefy_uuid      AS battlefy_uuid,
           |  t.season             AS tournament_season,
           |  d.id                 AS deck_id,
           |  d.eternalwarcry_link AS deck_link,
           |  m.participant_a_id,
           |  m.participant_b_id,
           |  m.participant_a_score,
           |  m.participant_b_score,
           |  m.round,
           |  m.bracket_name
           |FROM player p
           |  JOIN participant part ON p.id = part.player_id
           |  JOIN tournament t ON part.tournament_id = t.id
           |  JOIN deck d ON part.deck_id = d.id
           |  JOIN match m ON (part.id = m.participant_a_id OR part.id = m.participant_b_id)
           |WHERE p.id = $playerId
           |ORDER BY tournament_date""".stripMargin)
      while (rs.next()) {
        name = rs.getString("player_name")
        tournaments.+=((
          Tournament(rs.getString("battlefy_uuid"), rs.getString("tournament_name"), DateTime.parse(rs.getString("tournament_date")), Option(rs.getInt("tournament_season")), Some(rs.getInt("tournament_id"))),
          Score(
            rs.getInt("participant_id"),
            rs.getInt("participant_a_id"),
            rs.getInt("participant_b_id"),
            rs.getInt("participant_a_score"),
            rs.getInt("participant_b_score"),
            rs.getInt("round"),
            rs.getString("bracket_name")
          ),
          rs.getString("deck_link")))
      }
    } finally {
      conn.close()
    }

    val tournaments_info = tournaments.toList.groupBy(g => (g._1, g._3)).mapValues(v => v.map(_._2))
    val this_season_tournaments_info = tournaments_info.filter(_._1._1.season.contains(current_season))
    val info: mutable.ListBuffer[(String, String, String)] = new mutable.ListBuffer[(String, String, String)]()

    def winRate(inf: Map[(Tournament, String), List[Score]]) = inf.values.flatten.map(score => {
      val win = if (score.current_player_id == score.participant_a_id) score.participant_a_score else score.participant_b_score
      val loss = if (score.current_player_id == score.participant_a_id) score.participant_b_score else score.participant_a_score
      (win, loss)
    })

    def winrate_rounds(inf: Map[(Tournament, String), List[Score]]) = inf.values.flatten.map(score => {
      if (score.current_player_id == score.participant_a_id) score.participant_a_score > score.participant_b_score else score.participant_a_score < score.participant_b_score
    }).toList

    def times(number: Int): String = number match {
      case 0 => "-"
      case 1 => "once: "
      case 2 => "twice: "
      case _ => s"$number times: "
    }

    def max_elimination_round(tournament: String): Int = {
      eliminationRoundsCache.get(tournament) match {
        case Some(v) => v
        case None => val conn: Connection = database.getConnection()
          var name: String = ""
          try {
            val stmt = conn.createStatement
            val rs = stmt.executeQuery(
              s"""
                 |SELECT Max(m.round) as max_round
                 |FROM tournament t
                 |  JOIN participant p ON t.id = p.tournament_id
                 |  JOIN match m ON (p.id = m.participant_a_id OR p.id = m.participant_b_id)
                 |WHERE t.name = '$tournament' AND m.bracket_name = 'elimination'
               """.stripMargin)
            rs.next()
            val v = rs.getInt("max_round")
            eliminationRoundsCache.put(tournament, v)
            v
          } finally {
            conn.close()
          }
      }
    }

    def top8(inf: Map[(Tournament, String), List[Score]]) = inf.map(p => p._1 -> p._2.filter(s => s.bracket_name == "elimination" && s.round == (max_elimination_round(p._1._1.name) - 2))).filter(_._2.nonEmpty)

    def top4(inf: Map[(Tournament, String), List[Score]]) = inf.map(p => p._1 -> p._2.filter(s => s.bracket_name == "elimination" && s.round == (max_elimination_round(p._1._1.name) - 1))).filter(_._2.nonEmpty)

    def top2(inf: Map[(Tournament, String), List[Score]]) = inf.map(p => p._1 -> p._2.filter(s => s.bracket_name == "elimination" && s.round == max_elimination_round(p._1._1.name))).filter(_._2.nonEmpty)

    def winner(inf: Map[(Tournament, String), List[Score]]) = inf.map(p => p._1 -> p._2
      .filter { s =>
        s.bracket_name == "elimination" && s.round == max_elimination_round(p._1._1.name) &&
          (if (s.participant_a_id == s.current_player_id) s.participant_a_score > s.participant_b_score
          else s.participant_b_score > s.participant_a_score)
      })
      .filter(_._2.nonEmpty)

    val allGamesWon = winRate(tournaments_info).map(_._1).sum
    val allGamesLost = winRate(tournaments_info).map(_._2).sum
    val allGamesPlayed = allGamesWon + allGamesLost
    val allRoundsWon = winrate_rounds(tournaments_info).count(_ == true)
    val allRoundsLost = winrate_rounds(tournaments_info).count(_ == false)
    val allRoundsPlayed = allRoundsWon + allRoundsLost

    val tsGamesWon = winRate(this_season_tournaments_info).map(_._1).sum
    val tsGamesLost = winRate(this_season_tournaments_info).map(_._2).sum
    val tsGamesPlayed = tsGamesWon + tsGamesLost
    val tsRoundsWon = winrate_rounds(this_season_tournaments_info).count(_ == true)
    val tsRoundsLost = winrate_rounds(this_season_tournaments_info).count(_ == false)
    val tsRoundsPlayed = tsRoundsWon + tsRoundsLost


    val all_top8 = top8(tournaments_info)
    val all_top4 = top4(tournaments_info)
    val all_top2 = top2(tournaments_info)
    val all_winner = winner(tournaments_info)

    val ts_top8 = top8(this_season_tournaments_info)
    val ts_top4 = top4(this_season_tournaments_info)
    val ts_top2 = top2(this_season_tournaments_info)
    val ts_winner = winner(this_season_tournaments_info)

    def round(d: Double): Double = BigDecimal(d).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
    info.+=(("Tournaments played", this_season_tournaments_info.keySet.size.toString, tournaments_info.keySet.size.toString))

    info.+=(("Games: Win-Loss", s"$tsGamesWon - $tsGamesLost", s"$allGamesWon - $allGamesLost"))
    info.+=(("Games: Win-Loss %", s"${round(tsGamesWon * 100.0 / tsGamesPlayed)}% - ${round(tsGamesLost * 100.0 / tsGamesPlayed)}%", s"${round(allGamesWon * 100.0 / allGamesPlayed)}% - ${round(allGamesLost * 100.0 / allGamesPlayed)}%"))
    info.+=(("Rounds: Win-Loss", s"$tsRoundsWon - $tsRoundsLost", s"$allRoundsWon - $allRoundsLost"))
    info.+=(("Rounds: Win-Loss %", s"${round(tsRoundsWon * 100.0 / tsRoundsPlayed)}% - ${round(tsRoundsLost * 100.0 / tsRoundsPlayed)}%", s"${round(allRoundsWon * 100.0 / allRoundsPlayed)}% - ${round(allRoundsLost * 100.0 / allRoundsPlayed)}%"))
    info.+=(("Top 8", s"${times(ts_top8.size)}\n ${ts_top8.map(_._1._1.name).mkString("\n")}", s"${times(all_top8.size)}\n ${all_top8.map(_._1._1.name).mkString("\n")}"))
    info.+=(("Top 4", s"${times(ts_top4.size)}\n ${ts_top4.map(_._1._1.name).mkString("\n")}", s"${times(all_top4.size)}\n ${all_top4.map(_._1._1.name).mkString("\n")}"))
    info.+=(("Top 2", s"${times(ts_top2.size)}\n ${ts_top2.map(_._1._1.name).mkString("\n")}", s"${times(all_top2.size)}\n ${all_top2.map(_._1._1.name).mkString("\n")}"))
    info.+=(("Winner", s"${times(ts_winner.size)}\n ${ts_winner.map(_._1._1.name).mkString("\n")}", s"${times(all_winner.size)}\n ${all_winner.map(_._1._1.name).mkString("\n")}"))
    info.+=(("Decks played:",
      s"${this_season_tournaments_info.keySet.map(_._2).toList.sorted.mkString("\n")}",
      s"${tournaments_info.keySet.map(_._2).toList.sorted.mkString("\n")}"))
    (name, info.toList, tournaments_info.size <= 3)
  }
}
