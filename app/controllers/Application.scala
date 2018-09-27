package controllers

import java.util.concurrent.TimeUnit

import javax.inject.Inject
import org.joda.time.DateTime
import play.api.libs.json.{JsValue, _}
import play.api.libs.ws.WSClient
import play.api.mvc._
import types.Deck

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.language.implicitConversions
import scala.util.control.NonFatal

class Application @Inject()(ws: WSClient) extends Controller {

  def all_tournaments: () => String = () => "https://dtmwra1jsgyb0.cloudfront.net/organizations/5a0e00fdc4cd48033c0083b7/tournaments"

  def stage_info: String => String = (stage: String) => s"https://dtmwra1jsgyb0.cloudfront.net/stages/$stage/matches"

  def players: String => String = (tournament_id: String) => s"https://dtmwra1jsgyb0.cloudfront.net/tournaments/$tournament_id/teams"

  implicit def dateTimeOrdering: Ordering[DateTime] = Ordering.fromLessThan(_ isBefore _)


  def index = Action {
    Ok(views.html.index(getCurrentTournament))
  }

  def streamingResources(tournamentId: String) = Action {
    //TODO use tournament id
    Ok(views.html.streaming(getCurrentPlayersList))
  }

  def validateDeck(url: String) = Action {
    try {
      val deck = Await.result(ws.url(url.substring(0, Option(url.indexOf("?")).filterNot(_ < 0).getOrElse(url.length))).get().map(response => Deck.parse(url, response.body)), Duration.apply(30, TimeUnit.SECONDS))
      val result = deck.validate
      Ok(Json.obj("valid" -> result.isEmpty, "messages" -> result))
    } catch {
      case NonFatal(e) => Ok(Json.obj("valid" -> false, "messages" -> Json.arr(e.getMessage)))
    }
  }

  def validateDecks(tournamentId: String) = Action {
    val t = Await.result(ws.url(players(tournamentId)).get().map(response => {
      val list = Json.parse(response.body).asInstanceOf[JsArray].value.toList
      list
        .map(p => {
          val eternalName = (p \ "name").as[String]
          val customFields = (p \ "customFields" \\ "value").toList.map(_.as[String])
          (eternalName, customFields.find(_.contains("eternalwarcry")), customFields.filterNot(_.contains("eternalwarcry")).headOption)
        }
        )
    }), Duration.apply(30, TimeUnit.SECONDS))
    Ok(views.html.validation(t))
  }

  def generateDeckDoc(tournamentId: String) = Action {
    Ok("TODO")
  }

  def getCurrentPlayersList: (String, List[String]) = {
    Await.result(ws.url(all_tournaments()).get().flatMap(response => {
      val tournament = Json.parse(response.body).asInstanceOf[JsArray].value.toList
        .sortBy(o => DateTime.parse(o.\("startTime").as[String])).reverse.head
      val currentStage = tournament.\("stages").as[JsArray].value.toList.sortBy(o => DateTime.parse(o.\("startTime").as[String])).reverse.headOption
      if (currentStage.isEmpty) Future((tournament.\("name").as[String], List()))
      else {
        val stage = currentStage.get.\("_id").as[String]
        ws.url(stage_info(stage)).get().map(resp => {
          def name(node: JsValue): String = Json.arr(node.\("team").\\("customFields")).value.toList.map(_.\("value").as[String]).filter(_.contains("#")).head

          val rounds = Json.arr(Json.parse(resp.body)).value.toList.sortBy(o => o.\("roundNumber").as[String])
          val maxRound = rounds.map(r => r.\("roundNumber").as[Int]).max
          val round = rounds.filter(o => o.\("roundNumber").as[Int] == maxRound)
          (tournament.\("name").as[String], round.iterator.toList.map(r => name(r.\("top").get) + " - : - " + name(r.\("bottom").get)))

        })
      }
    }), Duration.apply(30, TimeUnit.SECONDS))
  }

  def getCurrentTournament: (String, String) = {
    implicit def dateTimeOrdering: Ordering[DateTime] = Ordering.fromLessThan(_ isBefore _)

    Await.result(ws.url(all_tournaments()).get().map(response => {
      val tournament = Json.parse(response.body).asInstanceOf[JsArray].value.toList.sortBy(o => DateTime.parse(o.\("startTime").as[String])).reverse.head
      (tournament.\("_id").as[String], tournament.\("name").as[String])
    }), Duration.apply(30, TimeUnit.SECONDS))
  }

  def generateImages(player1: (String, Option[String]), player2: (String, Option[String])): Unit = {
    println("test")
  }

  def generateResources = Action {
    request =>
      val body: Option[JsValue] = request.body.asJson
      body.flatMap(params => {
        (params \ "players").toOption.map(_.as[String]).map(_.split("[\\s-:]+")).map(players => {
          val scoreP1 = (params \ "p1score").toOption.map(_.as[String]).filterNot(_.equals("-"))
          val scoreP2 = (params \ "p2score").toOption.map(_.as[String]).filterNot(_.equals("-"))
          generateImages((players(0), scoreP1), (players(1), scoreP2)) //TODO What else?
          Ok("test")
        })
      }).getOrElse(NoContent)

  }
}
