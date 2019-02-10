package controllers

import java.nio.file.Files

import controllers.authentication.{SecureBackEnd, SecureView}
import controllers.discord.Discord
import javax.inject.Inject
import play.api.Configuration
import play.api.libs.json._
import play.api.mvc._
import types.Deck

import scala.language.implicitConversions
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
  private val SecureView = new SecureView(db)
  private val SecureBackEnd = new SecureBackEnd(db)

  def login = Action { request: Request[AnyContent] =>
    if (SecureBackEnd.isAuthorized(request)) Redirect("/")
    else Unauthorized(views.html.guest_view()).withHeaders("WWW-Authenticate" -> "Basic realm=Unauthorized")
  }

  def invalidateCache: Action[AnyContent] = Action {
    cache.invalidate()
    Ok("")
  }

  def logout = Action(Unauthorized(views.html.guest_view()).withHeaders("WWW-Authenticate" -> "Basic realm=Unauthorized"))

  def index = SecureView(Ok(views.html.index(battlefy.getCurrentTournament, authorized = true)))

  def validateDeck(url: String) = SecureBackEnd {
    try {
      val result = eternalWarcry.getDeck(url).validate
      Ok(Json.obj("valid" -> result.isEmpty, "messages" -> result))
    } catch {
      case NonFatal(e) => Ok(Json.obj("valid" -> false, "messages" -> Json.arr(e.getMessage)))
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
    graphics.generateImage((playersName, None), side, eternalWarcry.getDeck(link), Some(name)) match {
      case Right(file) =>

        discord.notifyAdmin(_.sendFile(file))
        discord.notifyStreamers(_.sendFile(file))

        val statsMessage = s"STATS: <https://eternal-tournaments.herokuapp.com/player?playerName=${playersName.split("[\\s\\+]")(0)}>"
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
      battlefy.currentRound.getOrElse(""),
      if (mainCam.exists(_.startsWith(player1._1))) player2 else player1,
      if (mainCam.exists(_.startsWith(player1._1))) player1 else player2
    )
    Ok(Files.readAllBytes(file.toPath)).withHeaders("Content-Type" -> "image/png",
      "content-disposition" -> s"""attachment; filename="${file.getName}"""")
  }

  def streaming = SecureView(Ok(views.html.streaming(battlefy.getCurrentTournament,
    battlefy.getCurrentTournament.battlefy_id, battlefy.currentOpponents)))

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
    val file = graphics.topPlayers(top10)

    discord.notifyAdmin(_.sendFile(file))
    discord.notifyStreamers(_.sendFile(file))

    Ok(Files.readAllBytes(file.toPath)).withHeaders("Content-Type" -> "image/png",
      "content-disposition" -> s"""attachment; filename="${file.getName}"""")
  }

  def invitationalPointsScene: Action[AnyContent] = SecureBackEnd {
    val points = db.invitationalPointsForCurrentSeason
    val winners = points.filter(_._3.nonEmpty)
    val top = points.filter(_._3.isEmpty).sortBy(_._2).reverse.take(24)
    val file = graphics.invitationalPoints(winners.sortBy(_._1) ++ (top ++ points
      .filterNot(p => winners.contains(p) || top.contains(p))
      .filter(p => p._2 == top.last._2))
      .sortBy(p => (top.head._2 - p._2, p._1.toLowerCase)))

    discord.notifyAdmin(_.sendFile(file))
    discord.notifyStreamers(_.sendFile(file))

    Ok(Files.readAllBytes(file.toPath)).withHeaders("Content-Type" -> "image/png",
      "content-disposition" -> s"""attachment; filename="${file.getName}"""")
  }

  def communityChampionshipPointsScene: Action[AnyContent] = SecureBackEnd {
    val points = db.communityChampionshipPointsResults
    val qualified = points.sortBy(_._2).reverse.take(16)
    val alsoQalified = points.filterNot(p => qualified.contains(p)).filter(_._2 == qualified.last._2)
    val file = graphics.communityChampionshipPoints((qualified ++ alsoQalified).sortBy(p => (qualified.head._2 - p._2, p._1.toLowerCase)))

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
          Json.obj("name" -> player._1, "deck" -> player._2.map(deck => Json.obj("name" -> deck.archetype, "url" -> deck.link, "list" -> deck.eternalFormat.mkString("\n"))).get)
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

  def checkInPage = Action {
    val tournament = battlefy.getCurrentTournament
    val authToken = (config.getString("battlefy.client.id"), config.getString("battlefy.auth0.client")) match {
      case (Some(client_id), Some(auth0Client)) => battlefy.getAuthToken(client_id, auth0Client)
      case _ => None
    }
    Ok(views.html.checkin(tournament, battlefy.listOfPlayers(tournament.battlefy_id), authToken))
  }

  def importTournament(battlefyUuid: String, season: Int, tournamentType: String) = SecureBackEnd {
    if (!db.existsTournament(battlefyUuid)) {
      val BYE = "BYE+0000"
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
        db.addParticipant(BYE, tournamentId, Deck.empty.link)
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
        (tournament \ "stages").as[JsArray].value.toList.par.foreach { stage =>
          val s_id = (stage \ "_id").as[String]
          val stageType = (stage \ "bracket" \ "type").as[String]
          battlefy.stageInfo(s_id).value.toList.foreach { game =>
            val roundNumber = (game \ "roundNumber").as[JsNumber].value.intValue()
            val player1Name = (game \ "top" \ "team" \ "name").as[String]
            val (player1Score, player2Name, player2Score) = if ((game \ "isBye").as[Boolean]) (2, BYE, 0) else (
              (game \ "top" \ "score").as[JsNumber].value.intValue(),
              (game \ "bottom" \ "team" \ "name").as[String],
              (game \ "bottom" \ "score").as[JsNumber].value.intValue()
            )
            db.importGame(tournamentId, player1Name, player2Name, player1Score, player2Score, roundNumber, stageType)
          }
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
      val points = db.invitationalPointsForCurrentSeason
      Ok(views.html.invitational_points(battlefy.getCurrentTournament, points, SecureView.isAuthorized(request)))
  }

  def customListOfCards = Action {
    Ok(views.html.custom_card_list(battlefy.getCurrentTournament))
  }

  def tournamentImport = Action {
    Ok(views.html.tournament_import(battlefy.getCurrentTournament))
  }
}