package controllers

import java.nio.file.Files

import javax.inject.Inject
import play.api.libs.json._
import play.api.mvc._
import types.Deck

import scala.language.implicitConversions
import scala.util.control.NonFatal

class Application @Inject()(
                             battlefy: Battlefy,
                             eternalWarcry: EternalWarcry,
                             fs: FileSystem,
                             db: DB, graphics:
                             Graphics,
                             docs: Docs) extends Controller {
  private val WithBasicAuth = new BasicAuthAction(db, battlefy)

  def index = Action {
    Ok(views.html.index(battlefy.getCurrentTournament))
  }

  def validateDeck(url: String) = Action {
    try {
      val result = eternalWarcry.getDeck(url).validate
      Ok(Json.obj("valid" -> result.isEmpty, "messages" -> result))
    } catch {
      case NonFatal(e) => Ok(Json.obj("valid" -> false, "messages" -> Json.arr(e.getMessage)))
    }
  }

  def validateDecks(tournamentId: String) = WithBasicAuth {
    Ok(views.html.validation(battlefy.listOfPlayers(tournamentId)))
  }

  def generateDeckDoc(tournamentId: String) = WithBasicAuth {
    Ok(views.html.deckdoc(battlefy.getTournament(tournamentId), battlefy.listOfPlayers(tournamentId).map { le =>
      (le._1, le._2.map(eternalWarcry.getDeck))
    }))
  }

  def doc(tournament_id: String): Action[AnyContent] = Action {
    val tournament = battlefy.getTournament(tournament_id)
    val exportFile = docs.doc(tournament, battlefy.listOfPlayers(tournament.battlefy_id).map(i => (i._1, i._2.map(eternalWarcry.getDeck).getOrElse(Deck.empty))))
    Ok(Files.readAllBytes(exportFile.toPath))
      .withHeaders("Content-Type" -> "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
        "content-disposition" -> s"""attachment; filename="${exportFile.getName}"""")
  }

  def side(side: String, link: String, name: String, player: String) = Action {
    eternalWarcry.changeDeckName(link, name)
    graphics.generateImage((player, None), side, eternalWarcry.getDeck(link), Some(name)) match {
      case Right(file) => Ok(Files.readAllBytes(file.toPath)).withHeaders("Content-Type" -> "image/png",
        "content-disposition" -> s"""attachment; filename="${file.getName}"""")
      case Left(error) => NotFound(error.getMessage)
    }
  }

  def left(link: String, name: String, player: String): Action[AnyContent] = side("left", link, name, player)

  def right(link: String, name: String, player: String): Action[AnyContent] = side("right", link, name, player)

  def playersStats = Action(statsPage)

  def statsPage = Ok(views.html.stats(db.getPlayers.toList.sortBy(_._2.toLowerCase)))

  def playerStats(playerId: Option[Int], playerName: Option[String] = None) = Action {
    def stat(id: Int) = {
      val (name, stats, isRookie) = db.playerStats(id)
      val list_of_players = battlefy.listOfPlayers(battlefy.getCurrentTournament.battlefy_id)
      val deck = list_of_players.filter(_._1 == name).filter(_._2.isDefined).map(_._2.get).map(link => eternalWarcry.getDeck(link)).headOption
      val opponentName = battlefy.currentOpponent(name)
      val opponentId = opponentName.flatMap(db.playerId)
      val previousGames: List[(String, String, String)] = opponentName.map(opponent => db.opponentPreviousInteraction(name, opponent)).getOrElse(List())
      Ok(views.html.player(name, deck, opponentName.map(n => (opponentId, n, list_of_players.filter(_._1 == n).filter(_._2.isDefined).map(_._2.get).map(link => eternalWarcry.getDeck(link)).headOption)), previousGames, stats, isRookie))
    }

    playerId match {
      case Some(id) => stat(id)
      case _ if playerName.isDefined => db.playerId(playerName.get).map(stat).getOrElse(statsPage)
      case _ => statsPage
    }
  }

  def sidePanel(player1Name: String, player1Score: Int, player1DeckName: String, player2Name: String, player2Score: Int, player2DeckName: String) = Action {
    val file = graphics.sidePanel(
      battlefy.getCurrentTournament.name,
      battlefy.currentRound.getOrElse(""),
      (if (player1Name.contains("+")) player1Name.split("\\+")(0) else player1Name, player1Score, player1DeckName),
      (if (player2Name.contains("+")) player2Name.split("\\+")(0) else player2Name, player2Score, player2DeckName)
    )
    Ok(Files.readAllBytes(file.toPath)).withHeaders("Content-Type" -> "image/png",
      "content-disposition" -> s"""attachment; filename="${file.getName}"""")
  }

  def streaming = WithBasicAuth(Ok(views.html.streaming(battlefy.currentOpponents)))

  def getOpponentsInfo(opponents: String) = Action {
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

  def generateCastersList(list: String) = Action {
    val file = graphics.casters(list)
    Ok(Files.readAllBytes(file.toPath)).withHeaders("Content-Type" -> "image/png",
      "content-disposition" -> s"""attachment; filename="${file.getName}"""")
  }
}