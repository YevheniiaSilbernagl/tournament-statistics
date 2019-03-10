package controllers

import java.nio.file.Files

import controllers.authentication.{SecureBackEnd, SecureView}
import controllers.discord.Discord
import javax.inject.Inject
import org.joda.time.LocalDate
import play.api.Configuration
import play.api.libs.json._
import play.api.mvc._
import types.{Deck, Score, Score_}

import scala.concurrent.duration._
import scala.language.{implicitConversions, postfixOps}
import scala.util.control.NonFatal

class Application @Inject()(
                             battlefy: Battlefy,
                             eternalWarcry: EternalWarcry,
                             fs: FileSystem,
                             db: DB,
                             graphics: Graphics,
                             docs: Docs,
                             config: Configuration,
                             discord: Discord,
                             cache: Cache
                           ) extends Controller {
  def guestView = views.html.guest_view(battlefy.getCurrentTournament.copy(currentStage = battlefy.currentStageId), authorized = false)

  private val SecureView = new SecureView(db, Results.Ok(guestView))
  private val SecureBackEnd = new SecureBackEnd(db)

  def login = Action { request: Request[AnyContent] =>
    if (SecureBackEnd.isAuthorized(request)) Redirect("/")
    else Unauthorized(guestView)
      .withHeaders("WWW-Authenticate" -> "Basic realm=Unauthorized")
  }

  def invalidateCache: Action[AnyContent] = Action {
    cache.invalidate()
    Ok("")
  }

  def logout = Action(Unauthorized(guestView)
    .withHeaders("WWW-Authenticate" -> "Basic realm=Unauthorized"))

  def index = SecureView {
    Ok(views.html.index(battlefy.getCurrentTournament, authorized = true))
  }

  def validateDeck(url: String) = SecureBackEnd {
    try {
      val result = eternalWarcry.getDeck(url).validate
      Ok(Json.obj("valid" -> result.isEmpty, "messages" -> result))
    } catch {
      case NonFatal(e) => Ok(Json.obj("valid" -> false, "messages" -> Json.arr(e.getMessage)))
    }
  }

  def cacheDeckName: Action[AnyContent] = Action {
    request =>
      request.body.asJson match {
        case Some((json: JsObject)) =>
          try {
            cache.put(json.value("link").as[String] + "_deck_name", json.value("name").as[String], 30 days)
          } catch {
            case NonFatal(e) =>
          }
          Ok("")
        case _ => BadRequest("not a json")
      }
  }

  def currentPairings = Action { request =>
    val tournament = battlefy.getCurrentTournament
    if (db.existsTournament(tournament.battlefy_id)) {
      Ok(views.html.current_pairings(tournament, List(), SecureView.isAuthorized(request)))
    } else {
      val players = battlefy.listOfPlayers(tournament.battlefy_id)
      val games = battlefy.games(tournament.battlefy_id).flatMap { r =>
        List(
          Score_(r._1, r._1, r._2, r._3, r._4, r._5, r._6),
          Score_(r._2, r._1, r._2, r._3, r._4, r._5, r._6)
        )
      }

      val currentRound = battlefy.currentRound

      def mapping(playerName: String): (String, Int, Int, String) = {
        (playerName,
          currentRound.map(round => {
            val (roundNumber, roundName) = round
            games
              .filter(p => playerName == p.current_player_name)
              .filterNot(s => s.round == roundNumber && s.bracket_name == roundName)
              .count(_.isWinner)
          }).getOrElse(0),
          currentRound.map(round => {
            val (roundNumber, roundName) = round
            games.filter(s => s.round == roundNumber && s.bracket_name == roundName)
              .filter(_.current_player_name == playerName)
              .map(s => if (s.participant_a_name == s.current_player_name) s.participant_a_score else s.participant_b_score).sum
          }).getOrElse(0),
          players.filter(_._1 == playerName).flatMap(_._2).headOption.map(eternalWarcry.getDeck).map(_.name).getOrElse(""))
      }

      val opponents = battlefy.currentOpponents.map(oo => (oo._1.map(mapping), oo._2.map(mapping)))
        .sortBy(oo => (oo._2.isDefined, oo._1.map(_._2).getOrElse(0) + oo._2.map(_._2).getOrElse(0))).reverse
      Ok(views.html.current_pairings(tournament, opponents, SecureView.isAuthorized(request)))
    }
  }

  def sendMessageToAllPlayers(message: String) = SecureBackEnd {
    battlefy.listOfPlayers(battlefy.getCurrentTournament.battlefy_id)
      .flatMap(_._3).map(_.split("#")(0)).distinct
      .foreach(player => discord.sendPM(player, message))
    Ok("Messages have been sent")
  }

  def validateDecks(tournamentId: String) = SecureView {
    Ok(views.html.validation(battlefy.getCurrentTournament, battlefy.listOfPlayers(tournamentId), discord.allPlayers))
  }

  def generateDeckDoc(tournamentId: String) = SecureView {
    Ok(views.html.deckdoc(battlefy.getTournament(tournamentId), battlefy.listOfPlayers(tournamentId).map { le =>
      (le._1, le._2.map(eternalWarcry.getDeck))
    }))
  }

  def expandedDeckDoc(tournament_id: String): Action[AnyContent] = SecureBackEnd {
    if (battlefy.getCurrentTournament.checkInStartTime
      .exists(checkInTime => checkInTime.toLocalDate.equals(LocalDate.now()))) {
      discord.talkToNightBot("@here Please update **decklists**")
    }
    val tournament = battlefy.getTournament(tournament_id)
    val exportFile = docs.expandedDeckDoc(tournament, battlefy.listOfPlayers(tournament.battlefy_id).map(i => (i._1, i._2.map(eternalWarcry.getDeck).getOrElse(Deck.empty))))
    Ok(Files.readAllBytes(exportFile.toPath))
      .withHeaders("Content-Type" -> "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
        "content-disposition" -> s"""attachment; filename="${exportFile.getName}"""")
  }

  def conciseDeckDoc(tournament_id: String): Action[AnyContent] = SecureBackEnd {
    val tournament = battlefy.getTournament(tournament_id)
    val exportFile = docs.conciseDeckDoc(tournament, battlefy.listOfPlayers(tournament.battlefy_id))
    Ok(Files.readAllBytes(exportFile.toPath))
      .withHeaders("Content-Type" -> "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
        "content-disposition" -> s"""attachment; filename="${exportFile.getName}"""")
  }

  def side(side: String, link: String, name: String, player: String) = SecureBackEnd {
    eternalWarcry.changeDeckName(link, name)
    val playersName = player.trim
    graphics.deckImage((playersName, None), side, eternalWarcry.getDeck(link), Some(name)) match {
      case Right(file) =>

        discord.notifyAdmin(_.sendFile(file))
        discord.notifyStreamers(_.sendFile(file))

        val statsMessage = s"STATS: <https://www.ets.to/player?playerName=${playersName.split("[\\s\\+]")(0)}>"
        discord.notifyAdmin(_.sendMessage(statsMessage))
        discord.notifyStreamers(_.sendMessage(statsMessage))

        Ok(Files.readAllBytes(file.toPath)).withHeaders("Content-Type" -> "image/png",
          "content-disposition" -> s"""attachment; filename="${file.getName}"""")
      case Left(error) => NotFound(error.getMessage)
    }
  }

  def oneDeckScene(link: String, name: String, player: String): Action[AnyContent] = SecureBackEnd {
    eternalWarcry.changeDeckName(link, name)
    val playersName = player.trim
    graphics.oneDeckImage((playersName, None), eternalWarcry.getDeck(link), Some(name)) match {
      case Right(file) =>

        discord.notifyAdmin(_.sendFile(file))
        discord.notifyStreamers(_.sendFile(file))

        val statsMessage = s"STATS: <https://www.ets.to/player?playerName=${playersName.split("[\\s\\+]")(0)}>"
        discord.notifyAdmin(_.sendMessage(statsMessage))
        discord.notifyStreamers(_.sendMessage(statsMessage))

        Ok(Files.readAllBytes(file.toPath)).withHeaders("Content-Type" -> "image/png",
          "content-disposition" -> s"""attachment; filename="${file.getName}"""")
      case Left(error) => NotFound(error.getMessage)
    }
  }

  def left(link: String, name: String, player: String): Action[AnyContent] = side("left", link, name, player)

  def right(link: String, name: String, player: String): Action[AnyContent] = side("right", link, name, player)

  def playersStats = Action {
    request =>
      statsPage(SecureView.isAuthorized(request))
  }

  def generateStats = SecureView(Ok(views.html.mass_stats(battlefy.getCurrentTournament)))

  def generateStatsForPlayers(players: String) = SecureBackEnd {
    val stats = players.split("\\n").map(_.trim).filterNot(_.isEmpty).distinct.map(p => p -> db.worldsStats(p)).toMap
    Ok(Json.obj("players" -> stats))
  }

  def statsPage(authorized: Boolean) = Ok(views.html.stats(battlefy.getCurrentTournament, db.getPlayers.toList.sortBy(_._2.toLowerCase), authorized))

  def playerStats(playerId: Option[Int], playerName: Option[String] = None) = Action {
    request =>
      def stat(id: Int) = {
        val (name, stats, isRookie) = db.playerStats(id)
        val list_of_players = battlefy.listOfPlayers(battlefy.getCurrentTournament.battlefy_id)
        val deck = list_of_players.filter(_._1 == name).filter(_._2.isDefined).map(_._2.get).map(link => eternalWarcry.getDeck(link)).headOption
        val opponentName = battlefy.currentOpponent(name)
        val opponentId = opponentName.flatMap(db.playerId)
        val previousGames: List[(String, String, String)] = opponentName.map(opponent => db.opponentPreviousInteraction(name, opponent)).getOrElse(List())
        val invitationalPoints = db.invitationalPointsPlayerCurrentSeason(id)
        val seriesPoints = db.seriesPoints(id)
        val opponent = opponentName.map(n => (opponentId, n, list_of_players.filter(_._1 == n).filter(_._2.isDefined)
          .map(_._2.get).map(link => eternalWarcry.getDeck(link)).headOption))
        val communityChampionshipPoints = db.communityChampionshipPoints(id)
        Ok(views.html.player(battlefy.getCurrentTournament, name, deck, opponent, previousGames, stats, isRookie,
          invitationalPoints, seriesPoints, communityChampionshipPoints, SecureView.isAuthorized(request)))
      }

      playerId match {
        case Some(id) => stat(id)
        case _ if playerName.isDefined => db.playerId(playerName.get).map(stat).getOrElse(statsPage(SecureView.isAuthorized(request)))
        case _ => statsPage(SecureView.isAuthorized(request))
      }
  }

  def sidePanel(player1Name: String, player1Score: Int, player1DeckName: String,
                player2Name: String, player2Score: Int, player2DeckName: String,
                mainCam: Option[String]) = SecureBackEnd {
    val player1 = (if (player1Name.contains("+")) player1Name.split("\\+")(0) else player1Name, player1Score, player1DeckName)
    val player2 = (if (player2Name.contains("+")) player2Name.split("\\+")(0) else player2Name, player2Score, player2DeckName)
    val file = graphics.sidePanel(
      battlefy.getCurrentTournament.name,
      battlefy.currentRoundTitle.getOrElse(""),
      if (mainCam.exists(_.startsWith(player1._1))) player2 else player1,
      if (mainCam.exists(_.startsWith(player1._1))) player1 else player2
    )
    Ok(Files.readAllBytes(file.toPath)).withHeaders("Content-Type" -> "image/png",
      "content-disposition" -> s"""attachment; filename="${file.getName}"""")
  }

  def streaming = SecureView(Ok(views.html.streaming(battlefy.getCurrentTournament, battlefy.currentOpponents)))

  def casters = SecureView(Ok(views.html.casters_panel(battlefy.getCurrentTournament)))

  def topCards(tournamentId: String): Action[AnyContent] = SecureBackEnd {
    val players = battlefy.listOfPlayers(tournamentId)
    val topCards = players.map(_._2).filter(_.isDefined).flatMap(l => eternalWarcry.getDeck(l.get).cards)
      .groupBy(_._1).map(p => p._1 -> p._2.map(_._2).sum).filterNot(_._1.isPower).map(p => p._1.name -> p._2).toList.sortBy(_._2).reverse.take(10)
    val file = graphics.topCards(topCards)

    discord.notifyAdmin(_.sendFile(file))
    discord.notifyStreamers(_.sendFile(file))

    Ok(Files.readAllBytes(file.toPath)).withHeaders("Content-Type" -> "image/png",
      "content-disposition" -> s"""attachment; filename="${file.getName}"""")
  }

  def customCardList(header: String, cards: List[String]): Action[AnyContent] = SecureBackEnd {
    val file = graphics.customCardList(header, cards)

    discord.notifyAdmin(_.sendFile(file))
    discord.notifyStreamers(_.sendFile(file))

    Ok(Files.readAllBytes(file.toPath)).withHeaders("Content-Type" -> "image/png",
      "content-disposition" -> s"""attachment; filename="${file.getName}"""")
  }

  def topPlayers(tournamentId: String): Action[AnyContent] = SecureBackEnd {
    val players = battlefy.listOfPlayers(tournamentId)
    val totalStats = players.map(_._1).flatMap(db.playerId).map(db.playerStats)
    val top10 = totalStats.map(p => (p._1, p._2(2)._4)).sortBy(_._2).reverse.take(10)
    val file = graphics.topPlayers(s"The Desk - Top ${top10.size} players this week", top10)

    discord.notifyAdmin(_.sendFile(file))
    discord.notifyStreamers(_.sendFile(file))

    Ok(Files.readAllBytes(file.toPath)).withHeaders("Content-Type" -> "image/png",
      "content-disposition" -> s"""attachment; filename="${file.getName}"""")
  }

  def customListOfPlayers(header: String, players: List[String]): Action[AnyContent] = SecureBackEnd {
    val totalStats = players.flatMap(db.playerId).map(db.playerStats)
      .sortBy(s => s._2(4)._4.split("-")(0)).reverse
    val file = graphics.statsList(s"The Desk - $header", totalStats)

    discord.notifyAdmin(_.sendFile(file))
    discord.notifyStreamers(_.sendFile(file))

    Ok(Files.readAllBytes(file.toPath)).withHeaders("Content-Type" -> "image/png",
      "content-disposition" -> s"""attachment; filename="${file.getName}"""")
  }

  def invitationalPointsScene: Action[AnyContent] = SecureBackEnd {
    val points = db.invitationalPointsForCurrentSeason
    val winners = points.filter(_._3.nonEmpty)
    val qualified = points.filter(_._3.isEmpty).sortBy(_._2).reverse
    val top = qualified.take(32 - winners.size)
    val currentPlayers = battlefy.listOfPlayers(battlefy.getCurrentTournament.battlefy_id).map(_._1)
    val havePotential = qualified.drop(winners.size).drop(top.size)
      .filter(_._2 >= (top.map(_._2).min - 2)).filter(p => currentPlayers.contains(p._1))

    val file = graphics.invitationalPoints((winners.sortBy(_._1.toLowerCase) ++ (top ++ points
      .filterNot(p => winners.contains(p) || top.contains(p))
      .filter(p => p._2 == top.last._2))
      .sortBy(p => (top.head._2 - p._2, p._1.toLowerCase))).map(r => (r._1, r._2, 0, r._3)),
      currentPlayers, havePotential.sortBy(_._1.toLowerCase).map(r => (r._1, r._2, 0, r._3)))

    discord.notifyAdmin(_.sendFile(file))
    discord.notifyStreamers(_.sendFile(file))
    Ok(Files.readAllBytes(file.toPath)).withHeaders("Content-Type" -> "image/png",
      "content-disposition" -> s"""attachment; filename="${file.getName}"""")
  }

  @Deprecated
  def invitationalPointsSceneOnlineUpdate: Action[AnyContent] = SecureBackEnd {
    val battlefyUuid = battlefy.getCurrentTournament.battlefy_id
    val listOfGames = if (db.existsTournament(battlefyUuid)) List.empty else battlefy.games(battlefyUuid)
    val currentGames = listOfGames.map { r =>
      val participant_a_id = db.getPlayerId(r._1).getOrElse(-1)
      val participant_b_id = db.getPlayerId(r._2).getOrElse(-1)
      Score(if (r._3 > r._4) participant_a_id else participant_b_id, participant_a_id, participant_b_id, r._3, r._4, r._5, r._6)
    }.filterNot(s => s.participant_a_score == 0 && s.participant_b_score == 0)
    val currentPlayers = listOfGames.flatMap(g => List(g._1, g._2)).distinct
    val currentRound = battlefy.currentRound

    def updatePoints(res: (String, Int, List[String])): (String, Int, Int, List[String]) = {
      if (currentGames.nonEmpty || currentPlayers.contains(res._1)) {
        val id = db.getPlayerId(res._1)
        val gamesWon = currentGames.filter(g => id.contains(g.current_player_id))
        val previouslyWon = gamesWon.filter { score =>
          currentRound match {
            case None => false
            case Some((round, bracket)) if round == 1 && bracket == "elimination" => score.bracket_name == "swiss"
            case Some((round, bracket)) if round == 1 && bracket == "swiss" => false
            case Some((round, bracket)) => score.bracket_name == bracket && score.round == round - 1
          }
        }
        (res._1, res._2 + previouslyWon.size, gamesWon.size - previouslyWon.size, res._3)
      } else {
        (res._1, res._2, 0, res._3)
      }
    }

    val points = db.invitationalPointsForCurrentSeason.map(updatePoints).sortBy(r => (r._2 + r._3, r._1.toLowerCase)).reverse

    val winners = points.filter(_._4.nonEmpty)
    val qualified = points.filter(_._4.isEmpty)
    val top = qualified.take(32 - winners.size)
    val alsoQualify = points
      .filterNot(p => winners.contains(p) || top.contains(p))
      .filter(p => (p._2 + p._3) == (top.last._2 + top.last._3))
    val allQualified = top ++ alsoQualify
    val havePotential = qualified.drop(allQualified.size)
      .filter(r => (r._2 + r._3) >= (top.map(r1 => r1._2 + r1._3).min - 2)).filter(p => currentPlayers.contains(p._1))
    val file = graphics.invitationalPoints(winners ++ allQualified, currentPlayers, havePotential)
    discord.notifyAdmin(_.sendFile(file))
    discord.notifyStreamers(_.sendFile(file))
    Ok(Files.readAllBytes(file.toPath)).withHeaders("Content-Type" -> "image/png",
      "content-disposition" -> s"""attachment; filename="${file.getName}"""")
  }

  def communityChampionshipPointsScene: Action[AnyContent] = SecureBackEnd {
    val points = db.communityChampionshipPointsResults.map(r => (r._1, r._2, 0))
    val qualified = points.sortBy(_._2).reverse.take(16)
    val alsoQalified = points.filterNot(p => qualified.contains(p)).filter(_._2 == qualified.last._2)
    val allQualified = qualified ++ alsoQalified
    val currentPlayers = battlefy.listOfPlayers(battlefy.getCurrentTournament.battlefy_id).map(_._1)
    val havePotential = points.drop(allQualified.size)
      .filter(_._2 >= (allQualified.map(_._2).min - 2)).filter(p => currentPlayers.contains(p._1))

    val file = graphics.communityChampionshipPoints(allQualified
      .sortBy(p => (qualified.head._2 - p._2, p._1.toLowerCase)),
      currentPlayers, havePotential)

    discord.notifyAdmin(_.sendFile(file))
    discord.notifyStreamers(_.sendFile(file))

    Ok(Files.readAllBytes(file.toPath)).withHeaders("Content-Type" -> "image/png",
      "content-disposition" -> s"""attachment; filename="${file.getName}"""")
  }

  @Deprecated
  def communityChampionshipPointsSceneOnlineUpdate: Action[AnyContent] = SecureBackEnd {
    val battlefyUuid = battlefy.getCurrentTournament.battlefy_id
    val listOfGames = if (db.existsTournament(battlefyUuid)) List.empty else battlefy.games(battlefyUuid)
    val games = listOfGames.map { r =>
      val participant_a_id = db.getPlayerId(r._1).getOrElse(-1)
      val participant_b_id = db.getPlayerId(r._2).getOrElse(-1)
      Score(if (r._3 > r._4) participant_a_id else participant_b_id, participant_a_id, participant_b_id, r._3, r._4, r._5, r._6)
    }.filterNot(s => s.participant_a_score == 0 && s.participant_b_score == 0)
    val currentPlayers = listOfGames.flatMap(g => List(g._1, g._2)).distinct
    val currentRound = battlefy.currentRound
    val currentRoundGames = games
      .filter(game => currentRound.map(_._1).contains(game.round))
      .filter(game => currentRound.map(_._2).contains(game.bracket_name))

    def updatePoints(res: (String, Int)): (String, Int, Int) = {
      if (games.nonEmpty || currentPlayers.contains(res._1)) {
        val id = db.getPlayerId(res._1)
        val gamesPlayed = games.filter(g => id.contains(g.current_player_id))
        val previouslyPlayed = gamesPlayed.filter { score =>
          currentRound match {
            case None => false
            case Some((round, bracket)) if round == 1 && bracket == "elimination" => score.bracket_name == "swiss" //todo  && currentRoundGames.isEmpty
            case Some((round, bracket)) if round == 1 && bracket == "swiss" => false
            case Some((round, bracket)) => score.bracket_name == bracket && score.round == round - 1
          }
        }
        val currentPoints = db.seriesPointsCalculation(Map((battlefy.getCurrentTournament, res._1) -> gamesPlayed))
        val previousPoints = db.seriesPointsCalculation(Map((battlefy.getCurrentTournament, res._1) -> previouslyPlayed))
        val currentPointsDifference = currentPoints.values.sum
        (res._1, res._2 + previousPoints.values.sum, currentPointsDifference - previousPoints.values.sum)
      } else {
        (res._1, res._2, 0)
      }
    }

    val points = db.communityChampionshipPointsResults.map(updatePoints).sortBy(r => (r._2 + r._3, r._1.toLowerCase)).reverse

    val qualified = points.take(16)
    val alsoQualified = points.filterNot(p => qualified.contains(p))
      .filter(r => (r._2 + r._3) == (qualified.last._2 + qualified.last._3))
    val allQualified = qualified ++ alsoQualified
    val havePotential = points.drop(allQualified.size)
      .filter(r => (r._2 + r._3) >= ((allQualified.last._2 + allQualified.last._3) - 2))
      .filter(p => currentPlayers.contains(p._1))

    val file = graphics.communityChampionshipPoints(
      allQualified,
      currentPlayers,
      havePotential)

    discord.notifyAdmin(_.sendFile(file))
    discord.notifyStreamers(_.sendFile(file))

    Ok(Files.readAllBytes(file.toPath)).withHeaders("Content-Type" -> "image/png",
      "content-disposition" -> s"""attachment; filename="${file.getName}"""")
  }

  def getOpponentsInfo(opponents: String) = SecureBackEnd {
    val tournamentPlayers = battlefy.listOfPlayers(battlefy.getCurrentTournament.battlefy_id)
    val players = opponents.split("-:-").toList.map(_.trim.split("\\s")(0)).map(n => if (n.contains("+")) n.split("\\+")(0) else n)
    val info = players.map(player => tournamentPlayers.find(p => p._1.startsWith(player)).map(p => (p._1, p._2.map(eternalWarcry.getDeck))))
    Ok(Json.obj("opponents" -> info.flatten.map(
      player =>
        if (player._2.isDefined)
          Json.obj("name" -> player._1, "deck" -> player._2.map(deck => Json.obj("name" -> deck.name, "url" -> deck.link, "list" -> deck.eternalFormat.mkString("\n"))).get)
        else Json.obj("name" -> player._1)
    )))
  }

  def sendMessage(user: String, message: String) = SecureBackEnd {
    discord.sendPM(user, message) match {
      case Some(_) => Ok("")
      case _ => NotAcceptable("User not found")
    }
  }

  def generateCastersList(list: String) = SecureBackEnd {
    val file = graphics.casters(list.split("\n").map(name => (name, discord.getAvatar(name))).toList)
    Ok(Files.readAllBytes(file.toPath)).withHeaders("Content-Type" -> "image/png",
      "content-disposition" -> s"""attachment; filename="${file.getName}"""")
  }

  def checkInPage: Action[AnyContent] = Action {
    val tournament = battlefy.getCurrentTournament
    val authToken = config.getString("battlefy.client.id").flatMap(battlefy.getAuthToken)
    Ok(views.html.checkin(tournament, battlefy.listOfPlayers(tournament.battlefy_id), authToken))
  }

  def importTournament(battlefyUuid: String, season: Int, tournamentType: String) = SecureBackEnd {
    if (!db.existsTournament(battlefyUuid)) {
      val tournament_ = battlefy.getTournamentInfo(battlefyUuid)
      if (tournament_.isEmpty) {
        BadRequest(s"Tournament $battlefyUuid not found")
      } else {
        val tournament = tournament_.get
        val tournamentName = (tournament \ "name").as[String]
        val tournamentId = (tournament \ "_id").as[String]
        val tournamentStartDate = (tournament \ "startTime").as[String]
        db.grantPrivileges()
        db.addTournament(tournamentName, tournamentStartDate, tournamentId, season, tournamentType)
        db.importDeck(Deck.empty)
        db.addParticipant(Battlefy.BYE, tournamentId, Deck.empty.link)
        battlefy.playersInfo(battlefyUuid).par.foreach { player =>
          val eternalName = (player \ "name").as[String]
          val customFields = (player \ "customFields").as[JsArray].value.toList.map(field => (field \ "value").as[String])
          val discordName = customFields.find(_.contains("#")).getOrElse(eternalName)
          val battlefyNames = (player \ "players").as[JsArray].value.toList.map(field => (field \ "username").as[String])
          val deckLinkO = customFields.find(_.contains("eternalwarcry"))
          val deck = deckLinkO.map(eternalWarcry.getDeck).getOrElse(Deck.empty)
          db.importDeck(deck)
          db.addPlayer(tournamentId, eternalName, discordName, battlefyNames, deck.link)
        }
        battlefy.games(battlefyUuid).par.foreach { game =>
          val (player1Name, player2Name, player1Score, player2Score, roundNumber, stageType) = game
          db.importGame(tournamentId, player1Name, player2Name, player1Score, player2Score, roundNumber, stageType)
        }
        Ok(s"Tournament $battlefyUuid has been successfully imported")
      }
    } else Conflict(s"Tournament $battlefyUuid exists")
  }

  def playerLifetimeWinRate(playerName: String) = SecureBackEnd {
    val name = playerName.split("\\+")(0)
    val file = graphics.trend(s"$name Lifetime Win Rate", db.lifeTimeWinRates(name))
    discord.notifyAdmin(_.sendFile(file))
    discord.notifyStreamers(_.sendFile(file))
    Ok(Files.readAllBytes(file.toPath)).withHeaders("Content-Type" -> "image/png",
      "content-disposition" -> s"""attachment; filename="${file.getName}"""")
  }


  def playerTrendingWinRate(playerName: String) = SecureBackEnd {
    val name = playerName.split("\\+")(0)
    val file = graphics.trend(s"$name Trending Win Rate", db.trendingWinRates(name))
    discord.notifyAdmin(_.sendFile(file))
    discord.notifyStreamers(_.sendFile(file))
    Ok(Files.readAllBytes(file.toPath)).withHeaders("Content-Type" -> "image/png",
      "content-disposition" -> s"""attachment; filename="${file.getName}"""")
  }

  def compareLifetimeWinRates(player1Name: String, player2Name: String) = SecureBackEnd {
    val name1 = player1Name.split("\\+")(0)
    val name2 = player2Name.split("\\+")(0)
    val file = graphics.compare(s"Lifetime Win Rate", (name1, db.lifeTimeWinRates(name1)), (name2, db.lifeTimeWinRates(name2)))
    discord.notifyAdmin(_.sendFile(file))
    discord.notifyStreamers(_.sendFile(file))
    Ok(Files.readAllBytes(file.toPath)).withHeaders("Content-Type" -> "image/png",
      "content-disposition" -> s"""attachment; filename="${file.getName}"""")
  }

  def compareTrendingWinRates(player1Name: String, player2Name: String) = SecureBackEnd {
    val name1 = player1Name.split("\\+")(0)
    val name2 = player2Name.split("\\+")(0)
    val file = graphics.compare(s"Trending Win Rate", (name1, db.trendingWinRates(name1)), (name2, db.trendingWinRates(name2)))
    discord.notifyAdmin(_.sendFile(file))
    discord.notifyStreamers(_.sendFile(file))
    Ok(Files.readAllBytes(file.toPath)).withHeaders("Content-Type" -> "image/png",
      "content-disposition" -> s"""attachment; filename="${file.getName}"""")
  }

  def seriesPoints = Action {
    request =>
      val points = db.seriesPointsResults
      Ok(views.html.series_points(battlefy.getCurrentTournament, points.filter(p => p._2._1 != 0 || p._2._2 != 0), SecureView.isAuthorized(request)))
  }

  def communityChampionshipPoints = Action {
    request =>
      val points = db.communityChampionshipPointsResults
      Ok(views.html.community_championship_points(battlefy.getCurrentTournament, points.filter(p => p._2 != 0), SecureView.isAuthorized(request)))
  }

  def invitationalPointsBreakDown(year: Int, season: Int) = Action {
    request =>
      val points = db.invitationalPoints(year, season).toList.sortBy(p => (p._2._2.values.sum, p._2._1)).reverse
      Ok(views.html.invitational_points_breakdown(battlefy.getCurrentTournament, points, SecureView.isAuthorized(request)))
  }

  def invitationalPoints: Action[AnyContent] = Action {
    request =>
      val points = db.invitationalPointsForCurrentSeason.sortBy(p => (-p._2, p._1.toLowerCase))
      Ok(views.html.invitational_points(battlefy.getCurrentTournament, points, SecureView.isAuthorized(request)))
  }

  def customListOfCards = SecureView {
    Ok(views.html.custom_card_list(battlefy.getCurrentTournament))
  }

  def tournamentImport = SecureView {
    Ok(views.html.tournament_import(battlefy.getCurrentTournament))
  }

  def howToRegister = Action {
    request => Ok(views.html.how_to_register(battlefy.getCurrentTournament, SecureView.isAuthorized(request)))
  }

  def dayOfTournament = Action {
    request => Ok(views.html.day_of_tournament(battlefy.getCurrentTournament, SecureView.isAuthorized(request)))
  }

  def tournamentCalendar = Action {
    request => Ok(views.html.tournament_calendar(battlefy.getCurrentTournament, SecureView.isAuthorized(request)))
  }

  def rules = Action {
    request => Ok(views.html.rules(battlefy.getCurrentTournament, SecureView.isAuthorized(request)))
  }

  def invitationalTournaments = Action {
    request => Ok(views.html.invitational_tournaments(battlefy.getCurrentTournament, SecureView.isAuthorized(request)))
  }
}