package controllers

import java.util.concurrent.TimeUnit

import com.fasterxml.jackson.databind.JsonNode
import javax.inject.Inject
import org.joda.time.DateTime
import play.api.libs.json.JsValue
import play.api.libs.ws.WSClient
import play.api.mvc._
import play.libs.Json

import scala.collection.JavaConversions._
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.language.implicitConversions

class Application @Inject()(ws: WSClient) extends Controller {

  def index = Action {
    Ok(views.html.index(getCurrentPlayersList))
  }

  def getCurrentPlayersList: (String, List[String]) = {
    implicit def dateTimeOrdering: Ordering[DateTime] = Ordering.fromLessThan(_ isBefore _)

    def all_tournaments: () => String = () => "https://dtmwra1jsgyb0.cloudfront.net/organizations/5a0e00fdc4cd48033c0083b7/tournaments"

    def stage_info: String => String = (stage: String) => s"https://dtmwra1jsgyb0.cloudfront.net/stages/$stage/matches"

    Await.result(ws.url(all_tournaments()).get().flatMap(response => {
      val tournament = Json.parse(response.body).iterator().toList
        .sortBy(o => DateTime.parse(o.get("startTime").asText)).reverse.head
      val stage = tournament.get("stages").iterator().toList.sortBy(o => DateTime.parse(o.get("startTime").asText)).reverse.head.get("_id").asText
      ws.url(stage_info(stage)).get().map(resp => {
        def name(node: JsonNode): String = node.get("team").get("customFields").iterator.toList.map(_.get("value").asText).filter(_.contains("#")).head

        val rounds = Json.parse(resp.body).iterator().toList.sortBy(o => o.get("roundNumber").asInt)
        val maxRound = rounds.map(r => r.get("roundNumber").asInt).max
        val round = rounds.filter(o => o.get("roundNumber").asInt == maxRound)
        (tournament.get("name").asText, round.iterator.toList.map(r => name(r.get("top")) + " - : - " + name(r.get("bottom"))))
      })
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
