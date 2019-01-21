package controllers

import java.sql.{Connection, Statement}

import javax.inject.Inject
import org.joda.time.DateTime
import org.joda.time.DateTime._
import play.api.db.Database
import play.api.mvc.Controller
import types.{Deck, Score, Tournament}

import scala.collection.mutable
import scala.concurrent.duration._

class DB @Inject()(database: Database, cache: Cache) extends Controller {

  val scorePointsCacheDuration: FiniteDuration = 1.day

  def winsOfTheSeason(id: Int): List[String] = {
    val (name, tournaments) = playerGames(id)
    val season = current_season
    val thisSeasonTournaments = tournaments
      .filter(_._1.date.year == DateTime.now().year)
      .filter(_._1.season.contains(season))
      .groupBy(g => (g._1, g._3)).mapValues(v => v.map(_._2))
    winner(thisSeasonTournaments).map(_._1._1.name).toList
  }

  def currentSeasonPlayers: Map[Int, String] = {
    val season = current_season
    val thisYear = now().year().get()
    val cacheKey = s"${thisYear}_$season"
    cache.get[Map[Int, String]](cacheKey).getOrElse {
      val query =
        s"""
           |SELECT
           |  p.id           AS id,
           |  p.eternal_name AS name
           |FROM player p
           |  JOIN participant part ON p.id = part.player_id
           |  JOIN tournament t ON part.tournament_id = t.id
           |WHERE t.season = $season AND t.date > date('$thisYear-01-01') AND p.eternal_name != 'BYE+0000'
       """.stripMargin
      val info: mutable.ListBuffer[(Int, String)] = new mutable.ListBuffer[(Int, String)]()
      val conn: Connection = database.getConnection()
      try {
        val stmt = conn.createStatement
        val rs = stmt.executeQuery(query)
        while (rs.next()) {
          info.+=((rs.getInt("id"), rs.getString("name")))
        }
      } finally {
        conn.close()
      }
      val map = info.toMap
      cache.put(cacheKey, map, scorePointsCacheDuration)
      map
    }
  }


  def seriesPoints(id: Int): (Int, Int) = {
    val cacheKey = s"series-points-$id"
    cache.get[(Int, Int)](cacheKey).getOrElse {
      val (name, tournaments) = playerGames(id)
      val thisYearTournaments = tournaments.filter(_._1.date.year == DateTime.now().year).groupBy(g => (g._1, g._3)).mapValues(v => v.map(_._2))
      val allTournaments = tournaments.groupBy(g => (g._1, g._3)).mapValues(v => v.map(_._2))

      def points(ts: Map[(Tournament, String), List[Score]]): Int = {
        val wins = winner(ts)
        val inTop2 = top2(ts).filterNot(e => wins.toList.map(_._1._1).contains(e._1._1))
        val inTop4 = top4(ts).filterNot(e => (wins.toList ++ inTop2.toList).map(_._1._1).contains(e._1._1))
        val inTop8 = top8(ts).filterNot(e => (wins.toList ++ inTop2.toList ++ inTop4.toList).map(_._1._1).contains(e._1._1))
        val undefeatedSwiss = ts.map(t => t._1 -> (t._2.filter(_.bracket_name == "swiss").count(s => !s.isWinner) == 0)).filter(_._2)
        val all = wins.map(t =>
          s"Winner - ${t._1._1.name}" -> 4) ++
          inTop2.map(t => s"Top 2 - ${t._1._1.name}" -> 3) ++
          inTop4.map(t => s"Top 4 - ${t._1._1.name}" -> 2) ++
          inTop8.map(t => s"Top 8 - ${t._1._1.name}" -> 1) ++
          undefeatedSwiss.map(g => s"Undefeated swiss - ${g._1._1.name}" -> 1)
        all.values.toList.sum
      }

      val result = (points(thisYearTournaments), points(allTournaments))
      cache.put(cacheKey, result, scorePointsCacheDuration)
      result
    }
  }

  def invitationalPointsCurrentSeason(id: Int): (Int, Map[String, Int]) = {
    val cacheKey = s"invitational-points-$id"
    cache.get[(Int, Map[String, Int])](cacheKey).getOrElse {
      val (name, tournaments) = playerGames(id)
      val thisYearTournaments = tournaments.filter(_._1.date.year == DateTime.now().year)
      val season = current_season
      val scores = thisYearTournaments.filter(_._1.season.contains(season)).groupBy(_._1)
        .map(g => (g._1.name, g._2.map(_._2).count(_.isWinner)))
      val points = (scores.values.toList.sorted.reverse.take(4).sum, scores)
      cache.put(cacheKey, points, scorePointsCacheDuration)
      points
    }
  }

  def lifeTimeWinRates(playerName: String): List[(DateTime, Double, Double)] = {
    implicit def dateTimeOrdering: Ordering[DateTime] = Ordering.fromLessThan(_ isBefore _)

    playerId(playerName) match {
      case Some(id) =>
        val tournaments = playerGames(id)._2.groupBy(_._1).map(p => (p._1.date, p._2.map(_._2))).toList.sortBy(_._1).reverse
        (for (tournament <- tournaments) yield {
          val tournamentsIncludingThis = tournaments.filterNot(_._1.isAfter(tournament._1))
          val scores = tournamentsIncludingThis.flatMap(_._2)
          val totalGames = scores.map(s => s.participant_a_score + s.participant_b_score).sum
          val gamesWon = scores.map(s => if (s.participant_a_id == s.current_player_id) s.participant_a_score else s.participant_b_score).sum
          val totalRounds = scores.length
          val roundsWon = scores.count(_.isWinner)
          (tournament._1, roundsWon.doubleValue() * 100 / totalRounds, gamesWon.doubleValue() * 100 / totalGames)
        }).sortBy(_._1)
      case _ => List()
    }
  }

  def trendingWinRates(playerName: String, window: Int = 4): List[(DateTime, Double, Double)] = {
    implicit def dateTimeOrdering: Ordering[DateTime] = Ordering.fromLessThan(_ isBefore _)

    playerId(playerName) match {
      case Some(id) =>
        val tournaments = playerGames(id)._2.groupBy(_._1).map(p => (p._1.date, p._2.map(_._2))).toList.sortBy(_._1).reverse
        (for (tournament <- tournaments) yield {
          val tournamentsIncludingThis = tournaments.filterNot(_._1.isAfter(tournament._1)).take(window)
          val scores = tournamentsIncludingThis.flatMap(_._2)
          val totalGames = scores.map(s => s.participant_a_score + s.participant_b_score).sum
          val gamesWon = scores.map(s => if (s.participant_a_id == s.current_player_id) s.participant_a_score else s.participant_b_score).sum
          val totalRounds = scores.length
          val roundsWon = scores.count(_.isWinner)
          (tournament._1, roundsWon.doubleValue() * 100 / totalRounds, gamesWon.doubleValue() * 100 / totalGames)
        }).sortBy(_._1)
      case _ => List()
    }
  }

  implicit def dateTimeOrdering: Ordering[DateTime] = Ordering.fromLessThan(_ isBefore _)

  val eliminationRoundsCache: mutable.HashMap[String, Int] = scala.collection.mutable.HashMap()

  def playerId(name: String): Option[Int] = {
    val conn: Connection = database.getConnection()
    try {
      val stmt = conn.createStatement
      val rs = stmt.executeQuery(s"SELECT id FROM player WHERE eternal_name LIKE '$name%'")
      if (rs.next())
        Some(rs.getInt("id")) else None
    } finally {
      conn.close()
    }
  }

  def worldsStats(player: String): Map[String, Map[String, String]] = {
    val allStats = playerId(player).map(playerStats(_)._2).getOrElse(List())
    List(
      allStats.find(_._1 == "Tournaments played"),
      allStats.find(_._1 == "Rounds: Win-Loss"),
      allStats.find(_._1 == "Number of premiere tournaments")
    ).flatMap(tuple =>
      tuple.map(t => List(
        "this_year " -> (t._1 -> t._3),
        "career " -> (t._1 -> t._4)
      ))).flatten.groupBy(_._1).map(p => p._1 -> p._2.map(_._2).toMap)
  }

  def pass(user: String): Option[String] = {
    val conn: Connection = database.getConnection()
    try {
      val stmt = conn.createStatement
      val rs = stmt.executeQuery(s"SELECT password_hash FROM user_ WHERE username='$user'")
      if (rs.next())
        Some(rs.getString("password_hash")) else None
    } finally {
      conn.close()
    }
  }

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
         |  t.name,
         |  t.date
         |FROM games g, opponent_id, tournaments t
         |WHERE (g.participant_a_id = opponent_id OR g.participant_b_id = opponent_id) AND t.id = g.tournament_id
       """.stripMargin
    val info: mutable.ListBuffer[(String, String, String)] = new mutable.ListBuffer[(String, String, String)]()
    val score: mutable.ListBuffer[(Int, Int)] = new mutable.ListBuffer[(Int, Int)]()
    val conn: Connection = database.getConnection()
    val playerShortName = if (playerName.contains("+")) playerName.substring(0, playerName.indexOf("+")) else playerName
    val opponentShortName = if (opponent.contains("+")) opponent.substring(0, opponent.indexOf("+")) else opponent
    try {
      val stmt = conn.createStatement
      val rs = stmt.executeQuery(query)
      while (rs.next()) {
        val participant_a_score = rs.getInt("participant_a_score")
        val participant_b_score = rs.getInt("participant_b_score")
        val participant_a_id = rs.getInt("participant_a_id")
        val opponent_id = rs.getInt("opponent_id")
        val tournament_name = rs.getString("name")
        val tournament_date = rs.getString("date")
        val round = rs.getInt("round")
        val bracket = rs.getString("bracket_name")
        info.+=((s"[$tournament_date] $tournament_name - $bracket round $round", s"$playerShortName - ${if (opponent_id == participant_a_id) participant_b_score else participant_a_score}", s"${if (opponent_id == participant_a_id) participant_a_score else participant_b_score} - $opponentShortName"))
        if (opponent_id == participant_a_id) {
          if (participant_a_score > participant_b_score) score.+=((0, 1)) else score.+=((1, 0))
        } else {
          if (participant_b_score > participant_a_score) score.+=((0, 1)) else score.+=((1, 0))
        }
      }
    } finally {
      conn.close()
    }

    if (info.isEmpty) List() else
      List((s"$playerShortName - ${score.toList.map(_._1).sum} : ${score.toList.map(_._2).sum} - $opponentShortName", "", ""), ("History: ", "", "")) ++ info.toList
  }

  def current_year: DateTime.Property = new DateTime().yearOfCentury()

  def current_season: Int = {
    val conn: Connection = database.getConnection()
    try {
      val stmt = conn.createStatement
      val rs = stmt.executeQuery("SELECT Max(season) as season FROM tournament t WHERE date_part('year', date) = date_part('year', CURRENT_DATE)")
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

  def playerGames(playerId: Int): (String, List[(Tournament, Score, String)]) = {
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
           |  t.tournament_type    AS tournament_type,
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
          Tournament(rs.getString("battlefy_uuid"), rs.getString("tournament_name"), DateTime.parse(rs.getString("tournament_date")), Option(rs.getInt("tournament_season")), Option(rs.getString("tournament_type")), Some(rs.getInt("tournament_id"))),
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
    (name, tournaments.toList)
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

  def times(number: Int): String = number match {
    case 0 => "-"
    case 1 => "once: "
    case 2 => "twice: "
    case _ => s"$number times: "
  }

  def top8(inf: Map[(Tournament, String), List[Score]]): Map[(Tournament, String), List[Score]] = inf.map(p => p._1 -> p._2.filter(s => s.bracket_name == "elimination" && s.round == (max_elimination_round(p._1._1.name) - 2))).filter(_._2.nonEmpty)

  def top4(inf: Map[(Tournament, String), List[Score]]): Map[(Tournament, String), List[Score]] = inf.map(p => p._1 -> p._2.filter(s => s.bracket_name == "elimination" && s.round == (max_elimination_round(p._1._1.name) - 1))).filter(_._2.nonEmpty)

  def top2(inf: Map[(Tournament, String), List[Score]]): Map[(Tournament, String), List[Score]] = inf.map(p => p._1 -> p._2.filter(s => s.bracket_name == "elimination" && s.round == max_elimination_round(p._1._1.name))).filter(_._2.nonEmpty)

  def winner(inf: Map[(Tournament, String), List[Score]]): Map[(Tournament, String), List[Score]] = inf.map(p => p._1 -> p._2
    .filter { s => s.bracket_name == "elimination" && s.round == max_elimination_round(p._1._1.name) && s.isWinner })
    .filter(_._2.nonEmpty)

  def premiere(inf: Map[(Tournament, String), List[Score]]): Int = inf.count(_._1._1.isPremiereEvent)


  def playerStats(playerId: Int): (String, List[(String, String, String, String)], Boolean) = {
    val (name, tournaments) = playerGames(playerId)
    val tournaments_info = tournaments.groupBy(g => (g._1, g._3)).mapValues(v => v.map(_._2))
    val this_year_tournaments_info = tournaments_info.filter(_._1._1.date.yearOfCentury() == current_year)
    val this_season_tournaments_info = tournaments_info.filter(t => t._1._1.season.contains(current_season) && t._1._1.date.yearOfCentury() == current_year)
    val info: mutable.ListBuffer[(String, String, String, String)] = new mutable.ListBuffer[(String, String, String, String)]()

    def winRate(inf: Map[(Tournament, String), List[Score]]) = inf.values.flatten.map(score => {
      val win = if (score.current_player_id == score.participant_a_id) score.participant_a_score else score.participant_b_score
      val loss = if (score.current_player_id == score.participant_a_id) score.participant_b_score else score.participant_a_score
      (win, loss)
    })

    def winrate_rounds(inf: Map[(Tournament, String), List[Score]]) = inf.values.flatten.map(_.isWinner).toList

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

    val tyGamesWon = winRate(this_year_tournaments_info).map(_._1).sum
    val tyGamesLost = winRate(this_year_tournaments_info).map(_._2).sum
    val tyGamesPlayed = tyGamesWon + tyGamesLost
    val tyRoundsWon = winrate_rounds(this_year_tournaments_info).count(_ == true)
    val tyRoundsLost = winrate_rounds(this_year_tournaments_info).count(_ == false)
    val tyRoundsPlayed = tyRoundsWon + tyRoundsLost


    val all_winner = winner(tournaments_info)
    val all_top2 = top2(tournaments_info).filterNot(d => all_winner.toList.map(_._1._1.name).contains(d._1._1.name))
    val all_top4 = top4(tournaments_info).filterNot(d => (all_winner.toList ++ all_top2.toList).map(_._1._1.name).contains(d._1._1.name))
    val all_top8 = top8(tournaments_info).filterNot(d => (all_winner.toList ++ all_top2.toList ++ all_top4.toList).map(_._1._1.name).contains(d._1._1.name))

    val ts_winner = winner(this_season_tournaments_info)
    val ts_top2 = top2(this_season_tournaments_info).filterNot(d => ts_winner.toList.map(_._1._1.name).contains(d._1._1.name))
    val ts_top4 = top4(this_season_tournaments_info).filterNot(d => (ts_winner.toList ++ ts_top2.toList).map(_._1._1.name).contains(d._1._1.name))
    val ts_top8 = top8(this_season_tournaments_info).filterNot(d => (ts_winner.toList ++ ts_top2.toList ++ ts_top4.toList).map(_._1._1.name).contains(d._1._1.name))

    val ty_winner = winner(this_year_tournaments_info)
    val ty_top2 = top2(this_year_tournaments_info).filterNot(d => ty_winner.toList.map(_._1._1.name).contains(d._1._1.name))
    val ty_top4 = top4(this_year_tournaments_info).filterNot(d => (ty_winner.toList ++ ty_top2.toList).map(_._1._1.name).contains(d._1._1.name))
    val ty_top8 = top8(this_year_tournaments_info).filterNot(d => (ty_winner.toList ++ ty_top2.toList ++ ty_top4.toList).map(_._1._1.name).contains(d._1._1.name))

    def round(d: Double): Double = if (d.isNaN) 0.0 else BigDecimal(d).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble

    info.+=(("Tournaments played", this_season_tournaments_info.keySet.size.toString, this_year_tournaments_info.keySet.size.toString, tournaments_info.keySet.size.toString))

    info.+=(("Games: Win-Loss",
      if (tsGamesPlayed == 0) s"-" else s"$tsGamesWon - $tsGamesLost",
      if (tyGamesPlayed == 0) s"-" else s"$tyGamesWon - $tyGamesLost", s"$allGamesWon - $allGamesLost"))
    info.+=(("Games: Win-Loss %",
      if (tsGamesPlayed == 0) s"-" else s"${round(tsGamesWon * 100.0 / tsGamesPlayed)}% - ${round(tsGamesLost * 100.0 / tsGamesPlayed)}%",
      if (tyGamesPlayed == 0) s"-" else s"${round(tyGamesWon * 100.0 / tyGamesPlayed)}% - ${round(tyGamesLost * 100.0 / tyGamesPlayed)}%",
      if (allGamesPlayed == 0) s"-" else s"${round(allGamesWon * 100.0 / allGamesPlayed)}% - ${round(allGamesLost * 100.0 / allGamesPlayed)}%"))
    info.+=(("Rounds: Win-Loss",
      if (tsGamesPlayed == 0) s"-" else s"$tsRoundsWon - $tsRoundsLost",
      if (tyGamesPlayed == 0) s"-" else s"$tyRoundsWon - $tyRoundsLost",
      s"$allRoundsWon - $allRoundsLost"))
    info.+=(("Rounds: Win-Loss %",
      if (tsRoundsPlayed == 0) s"-" else s"${round(tsRoundsWon * 100.0 / tsRoundsPlayed)}% - ${round(tsRoundsLost * 100.0 / tsRoundsPlayed)}%",
      if (tyRoundsPlayed == 0) s"-" else s"${round(tyRoundsWon * 100.0 / tyRoundsPlayed)}% - ${round(tyRoundsLost * 100.0 / tyRoundsPlayed)}%",
      if (allRoundsPlayed == 0) s"-" else s"${round(allRoundsWon * 100.0 / allRoundsPlayed)}% - ${round(allRoundsLost * 100.0 / allRoundsPlayed)}%"))
    info.+=(("Number of premiere tournaments",
      if (tsRoundsPlayed == 0) s"-" else s"${premiere(this_season_tournaments_info)}",
      if (tyRoundsPlayed == 0) s"-" else s"${premiere(this_year_tournaments_info)}",
      if (allRoundsPlayed == 0) s"-" else s"${premiere(tournaments_info)}"))
    info.+=(("Top 8",
      s"${times(ts_top8.size)}\n ${ts_top8.map(_._1._1.name).mkString("\n")}",
      s"${times(ty_top8.size)}\n ${ty_top8.map(_._1._1.name).mkString("\n")}",
      s"${times(all_top8.size)}\n ${all_top8.map(_._1._1.name).mkString("\n")}"))
    info.+=(("Top 4",
      s"${times(ts_top4.size)}\n ${ts_top4.map(_._1._1.name).mkString("\n")}",
      s"${times(ty_top4.size)}\n ${ty_top4.map(_._1._1.name).mkString("\n")}",
      s"${times(all_top4.size)}\n ${all_top4.map(_._1._1.name).mkString("\n")}"))
    info.+=(("Top 2",
      s"${times(ts_top2.size)}\n ${ts_top2.map(_._1._1.name).mkString("\n")}",
      s"${times(ty_top2.size)}\n ${ty_top2.map(_._1._1.name).mkString("\n")}",
      s"${times(all_top2.size)}\n ${all_top2.map(_._1._1.name).mkString("\n")}"))
    info.+=(("Winner",
      s"${times(ts_winner.size)}\n ${ts_winner.map(_._1._1.name).mkString("\n")}",
      s"${times(ty_winner.size)}\n ${ty_winner.map(_._1._1.name).mkString("\n")}",
      s"${times(all_winner.size)}\n ${all_winner.map(_._1._1.name).mkString("\n")}"))
    (name, info.toList, tournaments_info.size <= 3)
  }

  def importGame(tournamentId: String, player1Name: String, player2Name: String, player1Score: Int, player2Score: Int, round: Int, stageType: String): Unit = {
    insert("INSERT into match (participant_a_id, participant_b_id, participant_a_score, participant_b_score, round, bracket_name) VALUES\n" +
      "  ((select p.id from participant p JOIN player p2 ON p.player_id = p2.id JOIN tournament t ON p.tournament_id = t.id where t.battlefy_uuid=$$" + tournamentId + "$$ and p2.eternal_name=$$" + player1Name + "$$ limit 1),\n" +
      "   (select p.id from participant p JOIN player p2 ON p.player_id = p2.id JOIN tournament t ON p.tournament_id = t.id where t.battlefy_uuid=$$" + tournamentId + "$$ and p2.eternal_name=$$" + player2Name + "$$ limit 1),\n" +
      "    " + player1Score + ",\n" +
      "    " + player2Score + ",\n" +
      "    " + round + ",\n" +
      "    $$" + stageType + "$$\n" +
      "  )")
  }

  def addPlayer(tournamentId: String, eternalName: String, discordName: String, battlefyNames: List[String], deckLink: String): Unit = {
    insert("INSERT INTO player (eternal_name) VALUES ('" + eternalName + "') ON CONFLICT DO NOTHING")
    insert("INSERT into account (player_id, source, name)\n" +
      "VALUES\n" +
      "  ((select id from player where eternal_name='" + eternalName + "' LIMIT 1), 'DISCORD', '" + discordName + "'),\n" +
      "  ((select id from player where eternal_name='" + eternalName + "' LIMIT 1), 'ETERNAL', '" + eternalName + "')\n" +
      "ON CONFLICT DO NOTHING")
    battlefyNames.foreach { name => {
      insert("INSERT into account (player_id, source, name)\n" +
        "VALUES\n" +
        "  ((select id from player where eternal_name='" + eternalName + "' LIMIT 1), 'BATTLEFY', '" + name + "')" +
        "ON CONFLICT DO NOTHING")
    }
    }
    insert("insert into participant (player_id, tournament_id, deck_id) VALUES (\n" +
      "  (select id from player p where p.eternal_name=$$" + eternalName + "$$ limit 1),\n" +
      "  (select id from tournament t where t.battlefy_uuid=$$" + tournamentId + "$$ limit 1),\n" +
      "  (select id from deck d where d.eternalwarcry_link=$$" + deckLink + "$$ limit 1)\n" +
      ")")
  }

  def addParticipant(eternalName: String, tournamentId: String, deckLink: String): Unit = {
    insert("insert into participant (player_id, tournament_id, deck_id) VALUES (\n" +
      "  (select id from player p where p.eternal_name=$$" + eternalName + "$$ limit 1),\n" +
      "  (select id from tournament t where t.battlefy_uuid=$$" + tournamentId + "$$ limit 1),\n" +
      "  (select id from deck d where d.eternalwarcry_link=$$" + deckLink + "$$ limit 1)\n" +
      ")")
  }

  def importDeck(deck: Deck): Unit = {
    insert("INSERT INTO deck (eternalwarcry_link, name) VALUES ($$" + deck.link + "$$, $$" + deck.name + "$$) ON CONFLICT DO NOTHING")
    deck.mainDeck.foreach(card => {
      insert("INSERT INTO card (name) VALUES ($$" + card._1.name + "$$) ON CONFLICT DO NOTHING")
      insert("INSERT INTO pick (deck_id, card_id, number_of_cards, section) VALUES (\n" +
        "  (SELECT id FROM deck WHERE eternalwarcry_link = $$" + deck.link + "$$ LIMIT 1), \n" +
        "  (SELECT id FROM card WHERE name = $$" + card._1.name + "$$ LIMIT 1), " + card._2 + ", 'MAIN')")
    })
    deck.sideBoard.foreach(card => {
      insert("INSERT INTO card (name) VALUES ($$" + card._1.name + "$$) ON CONFLICT DO NOTHING")
      insert("INSERT INTO pick (deck_id, card_id, number_of_cards, section) VALUES (\n" +
        "  (SELECT id FROM deck WHERE eternalwarcry_link = $$" + deck.link + "$$ LIMIT 1), \n" +
        "  (SELECT id FROM card WHERE name = $$" + card._1.name + "$$ LIMIT 1), " + card._2 + ", 'SIDE')")
    })
    deck.market.foreach(card => {
      insert("INSERT INTO card (name) VALUES ($$" + card._1.name + "$$) ON CONFLICT DO NOTHING")
      insert("INSERT INTO pick (deck_id, card_id, number_of_cards, section) VALUES (\n" +
        "  (SELECT id FROM deck WHERE eternalwarcry_link = $$" + deck.link + "$$ LIMIT 1), \n" +
        "  (SELECT id FROM card WHERE name = $$" + card._1.name + "$$ LIMIT 1), " + card._2 + ", 'MARKET')")
    })
  }

  def addTournament(tournamentName: String, tournamentStartDate: String, tournamentId: String): Unit = {
    insert("INSERT INTO tournament (name, date, battlefy_uuid) VALUES ('" + tournamentName + "', '" + tournamentStartDate + "', '" + tournamentId + "')")
  }

  private def insert(sql: String): Unit = {
    var conn: Connection = null
    var stmt: Statement = null
    try {
      conn = database.getConnection()
      stmt = conn.createStatement
      stmt.executeUpdate(sql)
    } finally {
      if (stmt != null) stmt.close()
      if (conn != null) conn.close()
    }
  }
}
