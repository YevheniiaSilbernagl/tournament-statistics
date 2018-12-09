package controllers

import java.util.concurrent.TimeUnit

import javax.inject.Inject
import org.joda.time.DateTime
import play.api.libs.json._
import play.api.libs.ws.WSClient
import play.api.mvc.Controller
import types.Tournament

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

class Battlefy @Inject()(ws: WSClient) extends Controller {
  def currentOpponent(name: String): Option[String] = currentOpponents.filter(o => o._1.contains(name) || o._2.contains(name)).flatMap(o => if (o._1.contains(name)) o._2 else o._1).headOption

  def currentOpponents: List[(Option[String], Option[String])] = {
    val info = currentStageId.map(stageInfo(_).value.sortBy(_.asInstanceOf[JsObject].value("roundNumber").as[Int]).reverse).getOrElse(Seq())
    info.map(o => o.as[JsObject].value("top").as[JsObject] -> o.as[JsObject].value("bottom").as[JsObject])
      .map(o => (if (o._1.keys.contains("team")) Some(o._1) else None) -> (if (o._2.keys.contains("team")) Some(o._2) else None))
      .map(o => o._1.map(_.value("team").as[JsObject].value("name").as[String]) -> o._2.map(_.value("team").as[JsObject].value("name").as[String])).toList
  }

  def currentRound: Option[String] = {
    val round = currentStageId.map(stageInfo(_).value).toList.flatten.map(_.asInstanceOf[JsObject]).filter(_.value("top").asInstanceOf[JsObject].\\("team").nonEmpty).map(_.value("roundNumber").as[JsNumber].value.intValue())
    val bracket = currentStageInfo.map(_.as[JsObject].value("bracket").as[JsObject].value("type").as[JsString].value)
    if (round.isEmpty) None else Some(s"Round ${round.max}${if (bracket.isDefined) s" ${bracket.get.capitalize}" else ""}")
  }

  type EternalLink = String
  type EternalName = String
  type DiscordName = String

  def all_tournaments: () => String = () => "https://dtmwra1jsgyb0.cloudfront.net/organizations/5a0e00fdc4cd48033c0083b7/tournaments"

  def stage_info: String => String = (stage: String) => s"https://dtmwra1jsgyb0.cloudfront.net/stages/$stage/matches"

  def players: String => String = (tournament_id: String) => s"https://dtmwra1jsgyb0.cloudfront.net/tournaments/$tournament_id/teams"

  def listOfPlayers(tournamentId: String): List[(EternalName, Option[EternalLink], Option[DiscordName])] =
    Await.result(ws.url(players(tournamentId)).get().map(response => {
      val list = Json.parse(response.body).asInstanceOf[JsArray].value.toList
      list
        .map(p => {
          val eternalName = (p \ "name").as[String]
          val customFields = (p \ "customFields" \\ "value").toList.map(_.as[String])
          (eternalName, customFields.find(_.contains("eternalwarcry")), customFields.filterNot(_.contains("eternalwarcry")).headOption)
        }
        )
    }), Duration.apply(30, TimeUnit.SECONDS))

  def getTournament(battlefy_uuid: String): Tournament = {
    val t = getTournamentInfo(battlefy_uuid)
    Tournament(t.value("_id").as[String], t.value("name").as[String], DateTime.parse(t.value("startTime").as[String]), None, None)
  }

  def getTournamentInfo(battlefy_uuid: String): JsObject = {
    Await.result(ws.url(all_tournaments()).get().map(response => {
      Json.parse(response.body).asInstanceOf[JsArray].value.toList
        .map(_.as[JsObject]).find(t => t.value("_id").as[String] == battlefy_uuid)
        .get
    }), Duration.apply(30, TimeUnit.SECONDS))
  }

  def currentStageId: Option[String] = currentStageInfo.map(_.asInstanceOf[JsObject].value("_id").as[String])

  def currentStageInfo: Option[JsValue] = getTournamentInfo(getCurrentTournament.battlefy_id).value("stages").asInstanceOf[JsArray]
    .value.toList.sortBy(o => o.asInstanceOf[JsObject].value("startTime").as[String]).reverse.headOption

  def stageInfo(stage: String): JsArray = Await.result(ws.url(stage_info(stage)).get().map(response => {
    Json.parse(response.body).asInstanceOf[JsArray]
  }), Duration.apply(30, TimeUnit.SECONDS))

  def getCurrentTournament: Tournament = {
    implicit def dateTimeOrdering: Ordering[DateTime] = Ordering.fromLessThan(_ isBefore _)

    Await.result(ws.url(all_tournaments()).get().map(response => {
      val tournament = Json.parse(response.body).asInstanceOf[JsArray].value.toList.sortBy(o => DateTime.parse(o.\("startTime").as[String])).reverse.head
      val id = tournament.\("_id").as[String]
      val name = tournament.\("name").as[String]
      val date = DateTime.parse(tournament.\("startTime").as[String])
      val checkInStarted = tournament.\("checkInStartTime").toOption.map(v => DateTime.parse(v.as[String])).exists(_.isBeforeNow)
      Tournament(id, name, date, None, checkInStarted = checkInStarted, event_type = None)
    }), Duration.apply(30, TimeUnit.SECONDS))
  }
}
